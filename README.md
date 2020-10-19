# elm-scroll-spike

Scroll bars with custom logic in Elm.

Scroll bars are created in divs that are separate to the div that they control. Scroll 
events are captured and custom logic is used to display just the content that is
visible in the main div.

This is necessary if the content in the main div is very large - say a text buffer with
thousands or more lines.
