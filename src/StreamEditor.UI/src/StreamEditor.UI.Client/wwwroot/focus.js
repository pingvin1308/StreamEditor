FocusJS = {
    getTextWidth: function (lineText) {
        const text = document.createElement("span");
        document.body.appendChild(text);

        text.style.font = "monospace";
        text.style.height = 'auto';
        text.style.width = 'auto';
        text.style.position = 'absolute';
        text.style.whiteSpace = 'no-wrap';
        text.innerHTML = lineText;

        const width = Math.ceil(text.clientWidth);
        document.body.removeChild(text);
        return width;
    }
}