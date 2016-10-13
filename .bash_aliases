alias show-keyboard='gkbd-keyboard-display -g 1'
alias ipconfig='ifconfig'
alias python-webserver='python -m SimpleHTTPServer'
alias copy-to-clip='xclip -selection -c'
alias top-ten="history | sed 's/^ \+//;s/  / /' | cut -d' ' -f2- | awk '{ count[\$0]++ } END { for (i in count) print count[i], i }' | sort -rn |head -10"
alias start-android-notifier='~/modules/android-notifier-desktop/run.sh 2>&1 >> ~/modules/android-notifier-desktop/status.log  &'
alias intellij='/home/marcel/modules/idea-IC-141.1010.3/bin/idea.sh'
alias now='date "+%H:%M:%S  %d/%m/%y"'
alias appian-openvpn='sudo openvpn --script-security 2 --up /etc/openvpn/openvpn-dns --down /etc/openvpn/openvpn-dns --config ~/.openvpn/client.ovpn'
alias nano-crypto=crypto-file
alias rm-secure='shred -u -z'
alias g='git'
