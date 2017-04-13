# AdaRoombot

Simple serial control of iRobot Create 2 Roomba vacuum cleaner from Raspberry Pi.

## Build Instructions

- Get Raspberry Pi 2 (arm-linux-gnueabihf) toolchain from libre.adacore.com
http://libre.adacore.com/download/configurations#

- Follow instructions in compiler README to populate the compiler sysroot directory with the target C runtime files.

- Export the sysroot directory as ENV_PREFIX

- You can either open the project in GPS and build from there, or run
	$ gprbuild -Padaroombot.gpr

- Copy the built binary to your target
	$ scp obj/adaroombot pi@hostname.local:/home/pi

- Connect the USB serial cable to the Raspberry Pi and the Create2

- Determine which tty corresponds to the USB to serial cable (probably ttyUSB0). Look for something that says FTDI or Future TEchology Devices International
	$ lsusb

- Run the program
	$ ./adaroombot /dev/ttyUSB0

## How to add algorithms

The currently implemented algorithm is called Pong. It drives straight until it runs into something and then turns 180 degrees and drives straight again until it runs into something. 

To implement a new algorithm, copy the Pong algorithm and change the Process procedure. 
- Port is the serial port used to communicate with the robot
- Sensors is the collection of sensor data most recently retrieved from the robot



