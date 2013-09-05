Code accompanying the paper "Monadic Functional Reactive Programming"

The documentation is at http://homepages.cwi.nl/~ploeg/sfrpdocs/

To try out the drawing program do the following:

* git clone git://github.com/cwi-swat/symmetric-frp.git
* cd symmetric-frp
* cabal configure
* cabal build

Binary is now in dist/build/DrawProg/DrawProg

The program works as described in the paper. 
The tabs are controlled by mouse gestures: a mouse gesture is holding down the right button and moving left, right, up or down and then releasing the button

 * Mouse gesture up is duplicate the current tab
 * Mouse gesture down is delete current tab
 * Mouse gesture left is switch to tab on the right (this makes sense when making the movement)
 * Mouse gesture right is switch to tab on the left (this makes sense when making the movement)

Tab actions are super awesomely animated! 

At the start there is only one tab, so the only tab action that is possible is to duplicate it.

The code for :
  * the library is in Control/SFRP.hs
  * SDL interpreter is SDLSFRP.hs
  * the drawing program in the paper is in PaperExample.hs
  * the tabbed drawing program mentioned in the paper is in TabbedBoxes.hs
  
The code in extensible/ is work in progress on a new version based on heterogenous lists

