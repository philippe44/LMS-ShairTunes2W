This is a fork from https://github.com/disaster123/shairport2_plugin with the following changes
- works under Windows as well
- has a build-in mDNS server (the part below related to avahi does not appli anymore, or at least is not needed)
- allows audio to be sent to LMS sync'd players 
- caches artwork for LMS < 7.8 as well

Need to rebuild https://github.com/philippe44/TinySVCmDNS

==================== original readme ===========================

shairport2_plugin
================

ShairPort2 Plugin for Squeezebox Server adds airTunes support for each Squeezebox server client.

To install the plugin first - then install the dependancies:

Linux:

    > apt-get install libcrypt-openssl-rsa-perl libio-socket-inet6-perl libwww-perl avahi-utils libio-socket-ssl-perl
    > wget http://www.inf.udec.cl/~diegocaro/rpi/libnet-sdp-perl_0.07-1_all.deb
    > dpkg -i libnet-sdp-perl_0.07-1_all.deb

OSX:

    > sudo /usr/bin/perl -MCPAN -e'install Crypt::OpenSSL::RSA'
    > sudo /usr/bin/perl -MCPAN -e'install IO::Socket::INET6'
    > sudo /usr/bin/perl -MCPAN -e'install Net::SDP'


Now open the LMS GUI; click on Settings, then select the Plugins tab, at the bottom of the page add the repo:

http://raw.github.com/disaster123/shairport2_plugin/master/public.xml

Next install the plugin and enable as usual.

This version does not need a helper app in the global fs. It automatically detects the os and uses
the local helper.
  
Ensure avahi-daemon is configured correctly. edit the file /etc/avahi/avahi-daemon.conf:

    [server]
    use-ipv4=yes
    use-ipv6=yes
    
    [wide-area]
    enable-wide-area=yes
    
    [publish]
    publish-aaaa-on-ipv4=no
    publish-a-on-ipv6=no
    
    [reflector]
    
    [rlimits]
    rlimit-core=0
    rlimit-data=4194304
    rlimit-fsize=0
    rlimit-nofile=300
    rlimit-stack=4194304
    rlimit-nproc=3
  
Then restart avahi-daemon and LMS to apply all settings.

== THANKS ==

DMSTK - for doing the intial work and starting this great project
stuartUSA - for bringing it to the masses
chincheta0815 - for the OS autodetect and base work on metadata/covers
