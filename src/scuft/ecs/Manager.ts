import Entity from "./Entity";
import Component from "./Component";
import Category from "./Category";
import BitField from "./BitField";

export default class Manager {
    private entityIDs: Uint32Array = new Uint32Array(32);
    private entityPool: Entity[] = [];
    private maxCapacity = 0;
    private totalRecycled = 0;
    private lastRecycledIndex = -1;

    //Entity Management
    sizeOfEntities(): number {
        return this.maxCapacity - this.totalRecycled;
    }
    getEntity(entityID: number): Entity {
        if (entityID >= this.maxCapacity || this.entityIDs[entityID] != entityID) {
            throw "Entity id=" + entityID + " is not currently active and can't be retrieved";
        }
        if (this.entityPool[entityID] == null) {
            this.entityPool[entityID] = new Entity(entityID, this);
        }
        return this.entityPool[entityID];
    }
    createEntity(): Entity {
        let entityID;
        if (this.totalRecycled === 0) {
            if (this.maxCapacity >= this.entityIDs.length) {
                const newEntityIDs = new Uint32Array(this.maxCapacity + (this.maxCapacity >>> 1) + 1);
                newEntityIDs.set(this.entityIDs);
                this.entityIDs = newEntityIDs;
            }
            entityID = this.maxCapacity;
            this.entityIDs[this.maxCapacity] = this.maxCapacity;
            this.maxCapacity++;
        } else {
            console.log("::" + this.entityIDs + "::" + this.lastRecycledIndex + "::" + this.entityIDs[this.lastRecycledIndex]);
            let lastEntityID = this.entityIDs[this.lastRecycledIndex];
            entityID = this.entityIDs[this.lastRecycledIndex] = this.lastRecycledIndex;
            this.lastRecycledIndex = lastEntityID;
            this.totalRecycled--;
        }
        this.entityToComponentBits[entityID] = new BitField();
        return this.getEntity(entityID);
    }
    destroyEntity(entity: number | Entity): void {
        if (typeof entity !== "number") {
            entity = entity.id;
        }
        if (entity >= this.maxCapacity || this.entityIDs[entity] != entity) {
            throw "Entity id=" + entity + " is not currently active and can't be destroyed";
        }
        this.entityIDs[entity] = this.lastRecycledIndex;
        this.lastRecycledIndex = entity;
        this.totalRecycled++;
    }
    listEntities(): number[] {
        let toStringEntities: number[] = [];
        let counter = 0;
        for (let i = 0; i < this.maxCapacity; i++) {
            if (this.entityIDs[i] === i) {
                toStringEntities[counter++] = this.entityIDs[i];
            }
        }
        return toStringEntities;
    }

    //Component Management
    private singleComponentCategories: Category[] = [];
    private componentMap: Map<typeof Component, number> = new Map();
    private getComponentID(componentType: new() => Component): number {
        let typeID = this.componentMap.get(componentType);
        if (typeID === undefined) {
            typeID = this.componentMap.size;
            this.componentMap.set(componentType, typeID);
            this.singleComponentCategories.push(new Category())
        }
        return typeID;
    }
    attach<T extends Component>(entity: Entity | number, componentType: new() => T): T {
        if (typeof entity === "number") {
            entity = this.getEntity(entity);
        }
        let typeID = this.getComponentID(componentType);
        this.entityToComponentBits[entity.id].set(typeID);

        let componentInstance = new componentType();
        entity[componentType.name] = componentInstance;
        this.singleComponentCategories[typeID].notifyAttachObservers(entity.id);
        return componentInstance;
    }
    detach<T extends Component>(entity: Entity | number, componentType: new() => T): T {
        if (typeof entity === "number") {
            entity = this.getEntity(entity);
        }
        let componentInstance = entity[componentType.name];
        if (!componentInstance) {
            throw "Can't detach component type=" + componentType.name + " because entity id=" + entity.id +  " does not contain component type";
        }
        let typeID = this.getComponentID(componentType);
        this.entityToComponentBits[entity.id].set(typeID, false);

        delete entity[componentType.name];
        this.singleComponentCategories[typeID].notifyDetachObservers(entity.id);
        return <T> componentInstance;
    }

    //Category Management
    private categories: Category[] = [];
    private categoryBits: BitField[] = [];
    private entityToComponentBits: BitField[] = [];
    getCategory(...componentQuery: typeof Component[]): Category {
        if (componentQuery.length == 1) {
            return this.singleComponentCategories[this.getComponentID(componentQuery[0])];
        }
        let queriedBits = new BitField();
        let componentIDs = [];
        for (let i = 0; i < componentQuery.length; i++) {
            componentIDs[i] = this.getComponentID(componentQuery[i]);
            queriedBits.set(componentIDs[i]);
        }
        for (let i = 0; i < this.categories.length; i++) {
            if (this.categoryBits[i].equals(queriedBits)) {
                return this.categories[i];
            }
        }
        return this.newCategory(queriedBits, componentIDs);
    }
    private newCategory(queriedBits: BitField, componentQuery: number[]): Category {
        let category = new Category();
        for (let i = 0; i < this.maxCapacity; i++) {
            if (this.entityIDs[i] == i) { //Validate that entity id is active
                console.log("-----");
                console.log(this.entityToComponentBits[i]);
                if (this.entityToComponentBits[i].includes(queriedBits)) {
                    category.addInternalEntity(this.entityPool[i]);
                }
            }
        }
        this.categories.push(category); //Cache the category for future retrieval
        this.categoryBits.push(queriedBits);

        for (let i = 0; i < componentQuery.length; i++) { //Add dependency handlers to automatically manage category in the future
            let type = this.singleComponentCategories[componentQuery[i]];
            type.onComponentAttach(entityID => {
                if (!category.has(entityID) && this.entityToComponentBits[entityID].includes(queriedBits)) {
                    category.addInternalEntity(this.entityPool[i]);
                }
            });
            type.onComponentDetach(entityID => {
                if (category.has(entityID)) category.removeInternalEntity(entityID);
            });
        }
        return category;
    }
}