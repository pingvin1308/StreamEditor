SelectionJS = {
    getCurrentPosition: function () {
        const sel = document.getSelection();
        return sel ? sel.baseOffset : 0;
    },
    setOffset: function (offset) {
        const sel = document.getSelection();
        const container = document.getElementById("text");
        sel.setPosition(container, offset);
    },
    moveCharacter: function (direction) {
        const sel = document.getSelection();
        sel.modify("extend", direction, "character");
    },
    moveLine: function (direction) {
        const sel = document.getSelection();
        sel.modify("move", direction, "line");
    }
}