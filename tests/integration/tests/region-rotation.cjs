const test = require('node:test');
const assert = require('node:assert/strict');
const { createView, setSignal, markItems } = require('../vega.cjs');

test('circular plot rotation calculates region arc angles correctly', async testContext => {
    const vegaView = await createView(testContext);
    await setSignal(vegaView, 'circle_rotation', 0);

    // Arc angle shouldn't change during rotation; the bug was incorrect calculated arc angle at 85-87 degree rotations.
    const expectedArcAngles = new Map();
    for (const regionArc of markItems(vegaView, 'region_arc')) {
        expectedArcAngles.set(regionArc.datum.id, regionArc.endAngle - regionArc.startAngle);
    }

    for (const [regionId, expectedArcAngle] of expectedArcAngles) {
        assert.ok(
            Number.isFinite(expectedArcAngle) && expectedArcAngle > 0,
            `Region ${regionId} angle must be finite and positive`
        );
    }

    for (let rotationDegrees = 1; rotationDegrees < 360; rotationDegrees++) {
        await setSignal(vegaView, 'circle_rotation', rotationDegrees);

        const regionArcs = markItems(vegaView, 'region_arc');
        assert.equal(
            regionArcs.length,
            expectedArcAngles.size,
            `Region arc count changed at ${rotationDegrees} degrees`
        );

        for (const regionArc of regionArcs) {
            const arcAngle = regionArc.endAngle - regionArc.startAngle;

            assert.ok(
                Math.abs(arcAngle - expectedArcAngles.get(regionArc.datum.id)) < 1e-8,
                `Region ${regionArc.datum.id} angle changed at ${rotationDegrees} degrees`
            );
        }
    }
});
