const { readFileSync } = require('node:fs');

// run.R passes the specification file path and Vega library file path after the test filename.
const specFilePath = process.argv[2];
const vegaLibraryPath = process.argv[3];
const vegaLibrary = require(vegaLibraryPath);

// Creates and evaluates a Vega view without a browser.
async function createView(testContext) {
    const plotSpec = JSON.parse(readFileSync(specFilePath, 'utf8'));

    // Replace Vega's default error handler with one that throws so evaluation errors fail the test.
    const vegaLogger = vegaLibrary.logger();
    vegaLogger.error = evaluationError => {
        throw evaluationError;
    };

    const vegaView = new vegaLibrary.View(vegaLibrary.parse(plotSpec), { renderer: 'none', logger: vegaLogger });

    // Release the event listeners and timers when the test ends.
    testContext.after(() => vegaView.finalize());

    // Initialize and evaluate the plot.
    await vegaView.initialize().runAsync();

    return vegaView;
}

// Sets a Vega signal and re-evaluates the plot.
async function setSignal(vegaView, signalName, signalValue) {
    vegaView.signal(signalName, signalValue);
    await vegaView.runAsync();
}

// Returns the items for the given Vega mark in the plot's root group.
function markItems(vegaView, markName) {
    const marks = vegaView.scenegraph().root.items[0].items;
    return marks.find(topLevelMark => topLevelMark.name === markName).items;
}

module.exports = { createView, setSignal, markItems };
