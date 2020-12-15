export default class BitField {
    private static BITS_IN_BYTE = 8;
    private bitField: Uint8Array;

    constructor(numBytes = 0) {
        this.bitField = new Uint8Array(numBytes);
    }
    private assertSize(newSize: number): void {
        const newBitField = new Uint8Array(newSize + 1);
        newBitField.set(this.bitField);
        this.bitField = newBitField;
    }
    isEmpty(): boolean {
        for (let i = 0; i < this.bitField.length; i++) {
            if (this.bitField[i] != 0) return false;
        }
        return true;
    }
    set(index: number, bit = true): void {
        const byteIndex = index >>> 3;
        if (byteIndex >= this.bitField.length) this.assertSize(byteIndex);
        if (bit) {
            this.bitField[byteIndex] |= (1 << index % 8);
        } else {
            this.bitField[byteIndex] &= ~(1 << index % 8);
        }
    }
    get(index: number): boolean {
        const byteIndex = index >>> 3;
        if (byteIndex >= this.bitField.length) return false;
        return (this.bitField[byteIndex] & (1 << index)) !== 0;
    }
    includes(otherBitField: BitField): boolean {
        const minLen = Math.min(this.bitField.length, otherBitField.bitField.length);
        for (let i = 0; i < minLen; i++) {
            if ((otherBitField.bitField[i] & this.bitField[i]) != otherBitField.bitField[i]) return false;
        }
        for (let i = this.bitField.length; i < otherBitField.bitField.length; i++) {
            if (otherBitField.bitField[i] != 0) return false;
        }
        return true;
    }
    equals(otherBitField: BitField): boolean {
        let largeBitField;
        let smallBitField;
        if (this.bitField.length > otherBitField.bitField.length) {
            largeBitField = this.bitField;
            smallBitField = otherBitField.bitField;
        } else {
            largeBitField = otherBitField.bitField;
            smallBitField = this.bitField;
        }
        for (let i = 0; i < smallBitField.length; i++) {
            if (smallBitField[i] != largeBitField[i]) return false;
        }
        for (let i = smallBitField.length; i < largeBitField.length; i++) {
            if (largeBitField[i] != 0) return false;
        }
        return true;
    }
    indexOfLSB(): number {
        for (let i = 0; i < this.bitField.length; i++) {
            if (this.bitField[i] == 0) continue;
            let b = this.bitField[i] - 1;
            let c = (this.bitField[i] | b) ^ b;

            let index = 0;
            while (c > 1) {
                c = c >> 1;
                index++;
            }
            return index + i * BitField.BITS_IN_BYTE;
        }
        return -1;
    }
    getBinaryString(): string {
        let binaryString = "";
        for (let i = 0; i < this.bitField.length; i++) {
            let line = (this.bitField[i] >>> 0).toString(2);
            if (i + 1 < this.bitField.length) {
                let len = line.length;
                for (let j = 0; j < BitField.BITS_IN_BYTE - len; j++) {
                    line = "0" + line;
                }
            }
            binaryString = line + " " + binaryString;
        }
        return binaryString;
    }
}