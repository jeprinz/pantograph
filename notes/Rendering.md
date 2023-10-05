# Rendering

## How To Render A Buffer

1. Sync the buffer
   1. Sync each expr node with a dom element (via `elemId`)
2. Render the buffer
   1. Traverse the buffer, recursing on kids and arranging them into a final `Html` to yield using `renderer.arrangeExprKids`
3.  Hydrate the buffer
   1. Traverse the buffer:
      1. Annotate with `interactionStatus`: ... relating to cursor or selection
      2. Annotate with `onMouseDown`: ...
      3. Annotate with `onMouseUp`: ...

To update the hydration, update the synced buffer and then rehydrate.
