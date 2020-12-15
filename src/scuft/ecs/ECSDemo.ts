import Entity from "./Entity";
import Component from "./Component";
import Manager from "./Manager";

let manager: Manager = new Manager();

let entity = manager.createEntity();

class Transform extends Component {
    x: number = 33;
    y!: number;
}

class Sprite extends Component {
    imgPath!: string;
}

let t1 = entity.attach(Transform);
console.log(t1);

let t2 = entity.attach(Transform);
t2.x = 20;
t2.y = -20;
console.log(t2);

let s1 = entity.attach(Sprite);
s1.imgPath = "path1";
console.log(s1);

let s2 = entity.attach(Sprite);
s2.imgPath = "path2";
console.log(s2);

let t3 = entity.attach(Transform);
t3.x = 1;
t3.y = -1;
console.log(t3);

let catTest = manager.getCategory(Transform, Sprite);

let entity2 = manager.createEntity();
entity2.attach(Transform).x = 2;
entity2.attach(Sprite);
catTest.forEach(entity => {
    console.log(entity.Transform.x);
});

console.log("---------------------");

let catTest2 = manager.getCategory(Transform, Sprite);
entity2.detach(Sprite);
catTest2.forEach(entity => {
    console.log(entity.Transform.x);
});

export * from "./ECSDemo"