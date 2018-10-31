================================================================================
TESTING FOR :NIH
================================================================================

To test :nih functions, just change directories into the root of the
repository; then, load `t.lisp` from there, like so:

	[0]> (load "t/t.lisp")

Then, you can run the tests as you please:

	[1]> (nih-testing:do-all)
