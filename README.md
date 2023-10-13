# AirPlay to Logitech Media Server bridge
This is a fork from https://github.com/disaster123/shairport2_plugin with the following changes
- works under Windows as well
- has a build-in mDNS server (the part below related to avahi does not apply anymore, or at least is not needed)
- allows audio to be sent to LMS sync'd players 
- does not require (normally) any extra installation of Perl addons 

It is now part of LMS' 3rd party repositories, see support thread [here](
http://forums.slimdevices.com/showthread.php?106289-announce-ShairTunes2W-Airtunes-on-LMS-(forked-version-with-Windows-support))

Please see [here](https://github.com/philippe44/cross-compiling/blob/master/README.md#organizing-submodules--packages) to know how to rebuild my apps in general 

Otherwise, you can just get the source code and pre-built binaries:
```
cd ~
git clone http://github.com/philippe44/lms-shairtunes2w
cd ~/lms-shairtunes2w
git submodule update --init
```
and build doing:
```
cd ~/lms-shairtunes2w/application
make


