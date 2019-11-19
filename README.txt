===============================================================================
EKSD - let's go ahead and XXD again
===============================================================================

xxd is a very good hexdump program that makes editing files on UNIX very easy.
It also doesn't support text-tables. Which sucks.

eksd is a clone of a good hexdump program (even matching several arguments
exactly)… except it supports text-tables.


————————————————————————————————————————
USAGE
————————————————————————————————————————
To see a hexdump of a file, just run:
	$ eksd $FILE > $HEXDUMP

If you want to turn a hexdump (from eksd or xxd) back to a file:
	$ eksd -r $HEXDUMP > $FILE

And to make a hexdump with a custom text-table:
	$ eksd -t $TABLEFILE $FILE > $HEXDUMP

Text-tables are in a simple format— one hexcode per line, followed by it's
character. See text-tables/* for examples.

By default, eksd uses a built-in *fancy* text-table— it's basic ASCII,
except it'll print nice pictographics for newline characters, etc. These
require UTF, of course. If they don't work for you, use the "-a" arg to
revert to simple, non-fancy ASCII.
Note that specifying a text-table will override "-a", though.


————————————————————
EXAMPLES
————————————————————
Here's a part of Castlevania (EU) for the NES using it's text-table:

	$ eksd -t castle-table.txt castlevania.nes  | grep -A4 "18e80"
	00018e80: 5454 5454 866e 6854 6460 8486 5454 5454  ....THE.CAST....
	00018e90: 5454 5454 5466 8260 6488 7660 5454 5454  .....DRACULA....
	00018ea0: 5464 6e82 7084 867c 7e6e 6882 5462 6868  .CHRISTOPHER.BEE
	00018eb0: 5454 5454 5454 6668 6086 6e54 5454 5454  ......DEATH.....
	00018ec0: 5454 5462 6876 7c54 7688 6c7c 8470 5454  ...BELO.LUGOSI..

And here's that same file in xxd (just because I feel like showing off):

	$ xxd castlevania.nes | grep -A4 "18e80"
	00018e80: 5454 5454 866e 6854 6460 8486 5454 5454  TTTT.nhTd`..TTTT
	00018e90: 5454 5454 5466 8260 6488 7660 5454 5454  TTTTTf.`d.v`TTTT
	00018ea0: 5464 6e82 7084 867c 7e6e 6882 5462 6868  Tdn.p..|~nh.Tbhh
	00018eb0: 5454 5454 5454 6668 6086 6e54 5454 5454  TTTTTTfh`.nTTTTT
	00018ec0: 5454 5462 6876 7c54 7688 6c7c 8470 5454  TTTbhv|Tv.l|.pTT


————————————————————————————————————————
INSTALLATION
————————————————————————————————————————
Prebuilt binaries can be found at  https://mirror.eunichx.us/eksd/
for both Linux and OpenBSD.

… but if they don't work for you, you can make your own binary.
That requires a Lisp (I recommend SBCL) and Quicklisp (https://quicklisp.org).

Put this into "quicklisp/local-projects/", then run, in your lisp interpreter:

	# (ql:quickload '(eksd eksd-unix))
	# (save-lisp-and-die "eksd" :toplevel #'eksd-unix:invoke :executable t)

And bam, you've made a binary. Cool.


————————————————————————————————————————
BORING STUFF
————————————————————————————————————————
License is the GNU GPLv3:
       check COPYING.txt (/ipfs/QmTBpqbvJLZaq3hTMUhxX5hyJaSCeWe6Q5FRctQbsD6EsE)
Author is Jaidyn Ann <jadedctrl@teknik.io>
Sauce is at https://git.eunichx.us/eksd.git
