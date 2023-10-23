sudo apt-get update
sudo apt-get install -y --no-install-recommends xvfb libffi8 liblz4-1 zlib1g libgdk-pixbuf2.0-0 libglib2.0-0 libgtk2.0-0 libpangocairo-1.0-0 libpng16-16 libssl3
wget https://download.racket-lang.org/installers/8.10/racket-8.10-x86_64-linux-cs.sh -O racket.sh
chmod +x racket.sh
sudo ./racket.sh --unix-style --dest /usr/
rm -f ./racket.sh
#yes | raco pkg install racket-langserver