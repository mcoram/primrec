To try predict-extension. From the command line use:

  racket -t predict-extension.rkt -m

Or load and run predict-extension.rkt in DrRacket and execute "(main)"

Or try the web version. From the command line use:

  racket -t web.rkt

Then browse to http://localhost:8000/


To compute the primitive recursive function listings that it uses, run pr04 from the command line like so:

  racket -t pr04.rkt -m >out/functions-full.log

Browse the progress on the log with:

  tail -f out/functions-full.log

Also useful to check progress:

  grep extender out/functions-full.log

