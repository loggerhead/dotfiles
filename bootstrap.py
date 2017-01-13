#!/usr/bin/env python
import os
import errno
import shutil
import argparse


SOFTWARES = [
    ('brew', '/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"'),
    ('zsh vim macvim rg tmux pyenv', 'brew install zsh vim macvim ripgrep tmux pyenv'),
    ('oh-my-zsh', 'sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"'),
    ('zgen', 'git clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen"'),
]

EXCLUDES = [
    '.git',
    '.travis.yml',
    '.DS_Store',
    'bootstrap.py',
    'README.md',
    'LICENSE',
]


HOME = os.environ['HOME']
CURRENT_DIR = os.path.abspath(os.path.dirname(__file__))

FILES_PATHS = set(os.listdir(CURRENT_DIR))
FILES_PATHS.difference_update(set(EXCLUDES))
FILES_PATHS = sorted(list(FILES_PATHS))


def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('-f', '--force', action='store_true',
                        help='remove files if exists')
    parser.add_argument('-d', '--dry-run', action='store_true',
                        help='run without actually do')
    args = parser.parse_args()
    return vars(args)


def rm(path):
    if os.path.isdir(path):
        shutil.rmtree(path)
    else:
        os.remove(path)


def link(src, dst, overwrite=False, dry_run=False):
    print('{} -> {}'.format(src, dst))
    if dry_run:
        return

    try:
        os.symlink(src, dst)
    except OSError as e:
        if e.errno == errno.EEXIST:
            if overwrite:
                rm(dst)
                os.symlink(src, dst)
        else:
            raise e


def link_all(args):
    for f in FILES_PATHS:
        src = os.path.join(CURRENT_DIR, f)
        dst = os.path.join(HOME, f)
        link(src, dst,
             overwrite=args['force'],
             dry_run=args['dry_run'])

    # ln -s ~/.spacemacs.d ~/.emacs.d/private
    link(os.path.join(HOME, '.spacemacs.d'),
         os.path.join(HOME, '.emacs.d/private'),
         overwrite=args['force'],
         dry_run=args['dry_run'])


def run_command(s, dry_run=False):
    names, cmd = s
    print('installing {}...'.format(names))
    if dry_run:
        return
    os.system(cmd)


def install_all(args):
    for s in SOFTWARES:
        run_command(s, dry_run=args['dry_run'])


if __name__ == '__main__':
    args = get_args()
    install_all(args)
    link_all(args)
    run_command(('vim plugins', 'yes "" | vim +PluginInstall +qall'),
                dry_run=args['dry_run'])
