pkill whclan-bot ;
rm -rf . ;
git clone https://github.com/WH-Admins/whclan-bot . ;
nix-build && (source run.sh) ;
