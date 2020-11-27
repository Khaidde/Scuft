import PointLike from "./test"

let testPoint = {
    x: 1,
    y: 52
}
printPoint(testPoint);

function printPoint(point: PointLike) {
    console.log(point);
}