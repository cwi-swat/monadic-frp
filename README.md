Code accompanying the paper "Symmetric Functional Reactive Programming"

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
