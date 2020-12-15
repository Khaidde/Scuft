import Component from "./Component";
import Manager from "./Manager";

export default class Entity {
    id: number;
    private manager: Manager;
    [key: string]: any;
    constructor(id: number, manager: Manager) {
        this.id = id;
        this.manager = manager;
    }
    attach<T extends Component>(componentType: new() => T): T {
        return this.manager.attach(this, componentType);
    }
    detach<T extends Component>(componentType: new() => T): T {
        return this.manager.detach(this, componentType);
    }
}