import Entity from "./Entity";

export default class Category {
    entities: Entity[] = [];
    private attachCallbacks: ((entityID: number) => void)[] = [];
    private detachCallbacks: ((entityID: number) => void)[] = [];
    has(entityID: number): boolean{
        return !!this.entities[entityID];
    }
    addInternalEntity(entity: Entity) {
        this.entities[entity.id] = entity;
    }
    removeInternalEntity(entityID: number) {
        this.entities.splice(entityID, 1);
    }
    onComponentAttach(callback: (entityID: number) => void) {
        this.attachCallbacks.push(callback);
    }
    onComponentDetach(callback: (entityID: number) => void) {
        this.detachCallbacks.push(callback);
    }
    notifyAttachObservers(entityID: number) {
        for (let i = 0; i < this.attachCallbacks.length; i++) {
            this.attachCallbacks[i](entityID);
        }
    }
    notifyDetachObservers(entityID: number) {
        for (let i = 0; i < this.detachCallbacks.length; i++) {
            this.detachCallbacks[i](entityID);
        }
    }
    forEach(callback: (entity: Entity) => void) {
        for (let i = 0; i < this.entities.length; i++) {
            callback(this.entities[i]);
        }
    }
}