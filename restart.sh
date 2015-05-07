pkill whclan-bot ;
cd .. ; rm -rf whclan-bot ; 
git clone https://github.com/WH-Admins/whclan-bot ;
cd whclan-bot ;
nix-build && (source run.sh) ;
