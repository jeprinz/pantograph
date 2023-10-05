# Rendering

## How To Render A Buffer

1. Prerender the buffer
   1. Traverse the buffer:
      1. Annotate `elemId`: generated randomly
      2. Annotate with `interactionStatus`: ... relating to cursor or selection
      4. Annotate with `onMouseDown`: ...
      5. Annotate with `onMouseUp`: ...
2. Render the buffer
   1. Traverse the buffer, recursing on kids and arranging them into a final `Html` to yield using `renderer.arrangeExprKids`

