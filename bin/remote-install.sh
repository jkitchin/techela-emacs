#!/bin/bash
# sets up a remote techela repository. Needs one argument, the admin key, which
# should be already located in $HOME on the remote server.
#
# Usage: remote-install.sh  admin.pub

# Clone gitolote if needed
[ ! -d gitolite ]  && git clone https://github.com/jkitchin/gitolite

# make bin dir if needed
[ ! -d bin ] && mkdir bin

# install gitolite
gitolite/install --to $HOME/bin

# and set it up with the admin key
bin/gitolite setup -pk $1

echo "Finished setting up remote course."
#end

