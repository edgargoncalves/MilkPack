MilkPack
========

Homepage
--------
[http://sites.google.com/site/edgargoncalves/work/software/milkpack](http://sites.google.com/site/edgargoncalves/work/software/milkpack ) 

About it...
-----------
This open source project targets the average Remember the Milk user that lives within Mac OSX. It allows simple management of his/her tasks on a familiar Cocoa interface.

From the developer standpoint, the code is written in Common Lisp (I used Clozure CL, former OpenMCL, to allow easy access to the Objective-C frameworks). It also uses my RTM Lisp API library to access the remote task management interface. While this is my first project in Cocoa, feel free to take ideas from the code, or to provide any feedback you want.

How do I start using it?
------------------------
Fetch the latest released disk image, mount it, and drag MilkPack to some place you see fitting (e.g., the Applications folder). Then double-click on it. That was simple, right? Good.

How do I get its source to compile and make my own MilkPack build?
------------------------------------------------------------------
So you've got to the point that you tried Milkpack, didn't like something, talked to me about it, got a less than satisfactory reply, got angry and thought "hey, i can do it better myself!". Don't worry, that's probably not that hard :). And it is also a good sign. So pack your things, and get ready for the open source ride:

1. Fetch the code, using git: http://github.com/edgargoncalves/MilkPack
2. Install the latest Clozure CL (see their site for details on this procedure, if you run into trouble):
3. Make sure you have XCode. You'll need to install 10.4 support for Clozure CL to run properly, and you'll need InterfaceBuilder.
4. Read the code, make changes, mess arround.
5. To build it, see the MilkPack.lisp header, and follow the instructions.
6. Good luck, email me if you run into any problem. I'm on the ascending part of the learning curve, but I'm sure I'll come up with some advice.

To build the final application, until I make a nice script for it, you can execute the following line:
	$ open MilkPack.lisp -a /Applications/ccl/Clozure\ CL64.app

and press Shift-Apple-E to evaluate the entire file. wait for the IDE to close. Replace paths as approapriate, and don't forget to change the path defconstant form in MilkPack.lisp, too.

And as of now, you can also just use "make" on a shell, the included
Makefile will take care of everything compile and path-related for you (assuming you have a
$CCL environment variable properly set). You still need to read
MilkPack.lisp for instructions about the API key.

Has MilkPack got any long term plans?
-------------------------------------
In time I'd like to learn enough about Cocoa to produce a simple/fast interface for *every* Remember the Milk functionality supported by its API. I'd like to support some extra use cases, in particular full offline access to the entire application, and some Getting Things Done accelerators.

I like/hate MilkPack so much, I feel like helping. What can I do?
-----------------------------------------------------------------
Pick one or more from the following:

- Collaborate, provide patches, talk to me about it. Every feedback is good!
- Are you a design-oriented person? Great - I'm not! :) Contribute with icons, backgrounds, a logo, and every design conceptual idea you think it's appropriate.
- Promote this app on your circle of influence. The more people use it, the more Milkpack can learn and grow. (Help me help you!)
- This application is going to stay free and open source. That said, I accept donations, specially if they include drinks and/or other sorts of social entertainment. But I'll also accept financial contributions, don't want to make anybody angry! :) See homepage for a Paypal donation button.
