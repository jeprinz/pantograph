/*I got this from https://codepen.io/Zodiase/pen/qmjyKL*/

const selectTarget = (fromElement, selector) => {
  if (!(fromElement instanceof HTMLElement)) {
    return null;
  }

  return fromElement.querySelector(selector);
};

const resizeData = {
  tracking: false,
  startWidth: null,
  startCursorScreenX: null,
  handleWidth: 10,
  resizeTarget: null,
  parentElement: null,
  maxWidth: null,
};

$(document.body).on('mousedown', '.resize-handle--x', null, (event) => {
//$(document.body).on('mousedown', (event) => {
  if (event.button !== 0) {
    return;
  }

  event.preventDefault();
  event.stopPropagation();

  const handleElement = event.currentTarget;

  if (!handleElement.parentElement) {
    console.error(new Error("Parent element not found."));
    return;
  }

  // Use the target selector on the handle to get the resize target.
  const targetSelector = handleElement.getAttribute('data-target');
  const targetElement = selectTarget(handleElement.parentElement, targetSelector);

  if (!targetElement) {
    console.error(new Error("Resize target element not found."));
    return;
  }

  resizeData.startWidth = $(targetElement).outerWidth();
  resizeData.startCursorScreenX = event.screenX;
  resizeData.resizeTarget = targetElement;
  resizeData.parentElement = handleElement.parentElement;
  resizeData.maxWidth = $(handleElement.parentElement).innerWidth() - resizeData.handleWidth;
  resizeData.tracking = true;
});

$(window).on('mousemove', null, null, (event) => {
  if (resizeData.tracking) {
    const cursorScreenXDelta = event.screenX - resizeData.startCursorScreenX;
    const newWidth = Math.min(resizeData.startWidth - cursorScreenXDelta, resizeData.maxWidth);

    $(resizeData.resizeTarget).outerWidth(newWidth);
  }
});

$(window).on('mouseup', null, null, (event) => {
  if (resizeData.tracking) {
    resizeData.tracking = false;
  }
});