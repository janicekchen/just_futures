const scroller = scrollama();

function init() {
    scroller
        .setup({
            step: "#SCROLLERID article .step",
            offset: 0.5,
            debug: false
        
        })
        .onStepEnter(handleStepEnter)   
}