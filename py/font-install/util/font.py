import os
import shutil
import urllib2
import zipfile
from util import error
from util.config import Config
import fonts

__author__ = 'uliska'


class Font(object):
    """
    Object representing one single font.
    Capable of downloading archives and doing the 'installation'
    """
    def __init__(self, record):
        self._name = record['name']
        self._basename = record['basename']
        # font records usually only have a remote or a local version
        # as the version is eventually compared numerically
        # they are initialized with zero-string
        self._local_version = record.get('local_version', '0')
        self._remote_version = record.get('remote_version', '0')

        # existing font files in repo
        self._otf_files = []
        self._svg_files = [] # svg includes woff files

        # actions to be performed
        self._actions = {}

        # determine files and paths
        self.archive = os.path.join(Config.font_repo(), "{}.zip".format(self._basename))
        self.font_dir = os.path.join(Config.font_repo(), self._basename)
        self.otf_dir = os.path.join(self.font_dir, 'otf')
        self.svg_dir = os.path.join(self.font_dir, 'svg')


    def _archive_present(self):
        return os.path.isfile(self.archive)

    def _check(self):
        """
        Determine the necessary actions for the font,
        returning a dictionary with boolean values
        """

        # shortcut
        r = self._actions

        # download archive if
        # - font not declared locally
        # - remote is newer
        # - archive is missing locally
        r['download'] = False
        if not Config.local():
            r['download'] = True if (self._local_version == '0' or
                                 self._remote_newer() or
                                 not self._archive_present()) else False

        # extract archive if
        # - it (a new one) was downloaded
        # - the archive is present but not the target font directory
        r['extract'] = True if (r['download'] or
                                (self._archive_present() and not
                                    self._font_dir_present())) else False

        # the font is considered up to date if
        # - it doesn't have to be downloaded or extracted
        # - the font repo matches the links in the installation
        if (r['download'] or r['extract']):
            r['update_links'] = Config.lilypond_font_roots()
        else:
            r['update_links'] = []
            for l in Config.lilypond_font_roots():
                if not self._check_links(l):
                    r['update_links'].append(l)

        self._up_to_date = False if (r['download'] or
            r['extract'] or
            r['update_links']) else True


    def _check_links(self, font_root):
        """
        Determine if the list of font files matches the list of
        links in the given LilyPond installation's font directories
        """
        def compare_links(repo, install):
            """
            Return True if both string lists are equivalent.
            """
            if len(repo) != len(install):
                return False
            repo.sort()
            install.sort()
            for a, b in zip(repo, install):
                if a != b:
                    return False
            return True

        # collect lists of files in the repo and links in the LilyPond installation,
        # for both the otf and svg/woff directories
        otf_files = os.listdir(self.otf_dir)
        otf_target_dir = os.path.join(font_root, 'otf')
        otf_links = [f for f in os.listdir(otf_target_dir) if f.startswith(self._basename)]

        svg_files = os.listdir(self.svg_dir)
        svg_target_dir = os.path.join(font_root, 'svg')
        svg_links = [f for f in os.listdir(svg_target_dir) if f.startswith(self._basename)]

        # return True if both comparisons return True
        return (compare_links(otf_files, otf_links) and
            compare_links(svg_files, svg_links))


    def _download_archive(self):
        """
        Download the font archive to the local font repo
        """
        try:
            url = "{host}/{name}/{name}.zip".format(
                host=fonts.FONT_HOST,
                name=self._basename)
            print "  - Download ({}).".format(url)
            archive = urllib2.urlopen(url)
            output = open(self.archive, 'wb')
            output.write(archive.read())
            output.close()
            self._local_version = self._remote_version
            print "  ... OK"

        except Exception, e:
            error("Error downloading font archive for {}.\nMessage:\n{}".format(
                self._name, str(e)))

    def _extract_archive(self):
        """
        Extract the font's archive file to its own directory in the local font repo
        """
        def clear_directory(dir):
            """
            Ensure the target directory is empty
            """
            if os.path.exists(dir):
                shutil.rmtree(dir)
            os.mkdir(dir)

        print "  - Extract ({})".format(self.archive)
        archive = zipfile.ZipFile(self.archive, 'r')
        clear_directory(self.font_dir)
        archive.extractall(self.font_dir)

    def _font_dir_present(self):
        """
        Returns True if the extracted font directory is present.
        This doesn't check the contents of the directory in any way, however.
        """
        return os.path.isdir(self.font_dir)

    def _remote_newer(self):
        """
        Returns True if the remote version is newer than the local one
        """
        local = self._version_as_integer(self._local_version)
        remote = self._version_as_integer(self._remote_version)
        return remote > local

    def _update_links(self):
        """
        Add or update links in the LilyPond installation
        """
        def clear_directory(font_root, type, basename):
            """
            Remove existing links to a font in a dir to prevent inconsistencies.
            If physical copies are encountered the user is asked if he wants
            to remove them (and then if that decision should be remembered).
            In batch mode this would simply cause the program to fail.
            """
            target_dir = os.path.join(font_root, type)
            real_files = []
            for ln in os.listdir(target_dir):
                abs_ln = os.path.join(target_dir, ln)
                if ln.startswith(basename):
                    if os.path.islink(abs_ln):
                        os.unlink(abs_ln)
                    elif os.path.isfile(abs_ln):
                        real_files.append(abs_ln)

            if real_files:
                if Config.batch():
                    # the files will fail later when trying to link them
                    pass
                else:
                    # handle the presence of physical font files
                    if Config.remove_existing_files:
                        for f in real_files:
                            os.remove(f)
                    else:
                        print "The following font files are phyiscal copies:"
                        for f in real_files:
                            print " -", f
                        rem = raw_input("Do you want to remove them and replace them with links (y/anything)? ")
                        rem = True if rem.lower() == 'y' else False
                        if rem:
                            for f in real_files:
                                os.remove(f)
                            print "Done."
                        remember = raw_input("Do you want to remember this decision (y/anything)? ")
                        Config.remove_existing_files = rem if remember.lower() == 'y' else False

        def install_directory(font_root, type, basename):
            """Add links to all font files"""
            font_dir = os.path.join(self.font_dir, type)
            target_dir = os.path.join(font_root, type)
            link_targets = [f for f in os.listdir(font_dir) if f.startswith(self._basename)]
            for f in link_targets:
                target_name = os.path.join(font_dir, f)
                link_name = os.path.join(target_dir, f)
                try:
                    os.symlink(target_name, link_name)
                except OSError, e:
                    failed_links.append(target_name)

        print "  - Update links"
        failed_links = []
        for lily in self._actions['update_links']:
            for type in ['otf', 'svg']:
                clear_directory(lily, type, self._basename)
                install_directory(lily, type, self._basename)

        if not failed_links:
            print "  ... OK"
        return failed_links


    def _version_as_integer(self, version):
        """
        Calculate an integer representation of a font version string.
        """
        ver_list = version.split('.')
        power = 12
        value = 0
        for elem in ver_list:
            value += int(elem) * 10 ** power
            power -= 3
        return value

    def check(self):
        """
        Determine necessary actions and perform them
        """
        self._check()

    def handle(self):
        """
        Process necessary actions for the font
        (download, extract, update links)
        """
        if not self._up_to_date:
            print " -", self._name
        a = self._actions
        if a['download']:
            self._download_archive()
        if a['extract']:
            self._extract_archive()
        if a['update_links']:
            failed_links = self._update_links()
            if failed_links:
                print ("Some links for font {} could not be installed.\n" +
                       "Maybe you have already installed actual font files?").format(self._name)
                print '\n'.join(failed_links)

    def merge_font(self, record):
        """
        Merge a font record into an existing Font object.
        """

        # we can consider 'name' to be identical,
        # otherwise this wouldn't be executed.
        if 'basename' in record and not self._basename == record['basename']:
            error("Conflicting font record found:\n  {}".format(record))

        # We _assume_ this will work because from reading the catalogs
        # a record can have only a remote _or_ a local version.
        if self._remote_version == '0':
            self._remote_version = record.get('remote_version', '0')
        if self._local_version == '0':
            self._local_version = record.get('local_version', '0')
