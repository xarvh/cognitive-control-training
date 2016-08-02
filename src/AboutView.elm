module AboutView exposing (..)

import Markdown
import Html exposing (..)
import Html.Attributes exposing (href)


view =
    Markdown.toHtml [] """

Introduction
------------

Digging myself out of depression is easily the most difficult thing I have ever done in my life.
It has taken me twenty years of painful effort and struggle, and thanks to the social stigma of mental
illness, it was a very lonely journey.

Despite all that, it was worth it.

The two tasks provided in this page helped me significantly in dealing with depression and anhedonia.
I provide them in the hope that they will be useful for others.



Cognitive Control Training
--------------------------

There is
[limited](http://www.ncbi.nlm.nih.gov/pubmed/24589123)
but
[encouraging](https://www.researchgate.net/profile/Michael_Thase/publication/225563390_Neurobehavioral_Therapies_in_the_21st_Century_Summary_of_an_Emerging_Field_and_an_Extended_Example_of_Cognitive_Control_Training_for_Depression/links/56e68ecb08ae65dd4cc1a616.pdf)
evidence for the usefulness of cognitive control training, but I am neither
a doctor nor a researcher: your use of this application is solely your responsibility and I encourage
you to
[research the subject](https://scholar.google.com.au/scholar?q=cognitive+control+training)
[yourself](http://sci-hub.cc/).

At least in my experience, curing depression required a combination of several different treatments, and
cognitive control training was only one of them.
Among those I personally found significantly effective:

- Cognitive behavioural therapy
- Medication (mirtazapine, sertraline)
- Mindfulness training
- Intense physical exercise
- Exposure to sunlight
- Transcranial magnetic stimulation

I encourage you to research and experiment with different treatments, as long as you do it under
the supervision of your doctor.

If you can, take part in clinical trials and help to push the science forward.



The App
-------

This app does not "send home" any information whatsoever, and in fact you can
[download it](https://github.com/xarvh/cortex/archive/gh-pages.zip) (open `index.html`) and use it
without an internet connection.


While the two tasks are completely functional and can be used already, I will keep improving the page.

For any technical issues or if you want to contribute, visit the [GitHub page](https://github.com/xarvh/cortex).
"""

