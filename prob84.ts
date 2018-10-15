
class Numerator {
    static num: number = 0;
    static next() {
        return this.num++;
    }
}

class S {
    code: String;
    num: number;
    counter: number = 0;
    constructor(code) {
        this.code = code;
        this.num = Numerator.next();
    }

    fullnum():string {
        return this.num < 10 ? `0${this.num}` : `${this.num}`;
    }

    landed():void {
        this.counter += 1;
    }
}

class Dice {
    count: number;
    sides: number;
    history: number[];
    constructor(count, sides) {
        this.count = count;
        this.sides = sides;
        this.history = [];
    }
    roll():number {
        let result: number = 0;
        for (let i = 0; i < this.count; i++) {
            result += Math.floor(Math.random() * this.sides) + 1;
        }
        if (this.history.push(result) > 3) {
            this.history.pop();
        }
        return result;
    }
    threeDoubles():boolean {
        let sum = 0;
        for (let i = 0; i < this.history.length; i++) {
            sum += this.history[i];
        }
        let result = sum === this.count * this.sides * 3;
        return result;
    }
}

class Deck {
    cards: number[];
    constructor(size) {
        this.cards = [];
        for (let i = 1; i <= size; i++) {
            this.cards.push(i);
        }
        // Shuffle.
        for (let i = this.cards.length - 1; i > 0; i--) {
            const j = Math.floor(Math.random() * (i + 1));
            [this.cards[i], this.cards[j]] = [this.cards[j], this.cards[i]];
        }
    }
    next():number {
        let result = this.cards.shift();
        this.cards.push(result);
        return result;
    }
}

function simulate(dices, sides):string {
    let field = [new S('GO'), new S('A1'), new S('CC1'), // 2
                new S('A2'), new S('T1'), new S('R1'), // 5
                new S('B1'), new S('CH1'), new S('B2'), // 8
                new S('B3'), new S('JAIL'), new S('C1'), // 11
                new S('U1'), new S('C2'), new S('C3'), // 14
                new S('R2'), new S('D1'), new S('CC2'), // 17
                new S('D2'), new S('D3'), new S('FP'), // 20
                new S('E1'), new S('CH2'), new S('E2'), // 23
                new S('E3'), new S('R3'), new S('F1'), // 26
                new S('F2'), new S('U2'), new S('F3'), // 29
                new S('G2J'), new S('G1'), new S('G2'), // 32
                new S('CC3'), new S('G3'), new S('R4'), // 35
                new S('CH3'), new S('H1'), new S('T2'), // 38
                new S('H2')
            ];
    let chanceDeck = new Deck(16);
    let ccDeck = new Deck(16);
    let dice:Dice = new Dice(dices, sides);
    let position:number = 0;
    let moves:number = 0;

    for (let i = 0; i < 10000000; i++) {
        position = (position + dice.roll()) % field.length;

        if (dice.threeDoubles() || field[position].code === 'G2J') {
            position = 10; // Jail.
        } else if (field[position].code.search('CC') === 0) {
            // Community chest.
            let card = ccDeck.next();
            switch (card) {
                case 1:
                    position = 0; // Go.
                    break;
                case 2:
                    position = 10; // Jail.
                    break;
            }
        } else if (field[position].code.search('CH') === 0) {
            // Chance.
            let card = chanceDeck.next();
            switch (card) {
                case 1:
                    position = 0; // Go.
                    break;
                case 2:
                    position = 10; // Jail.
                    break;
                case 3:
                    position = 11; // C1.
                    break;
                case 4:
                    position = 24; // E3.
                    break;
                case 5:
                    position = 39; // H2.
                    break;
                case 6:
                    position = 5; // R1.
                    break;
                case 7:
                case 8:
                    while (field[position].code[0] !== 'R') {
                        position = (position + 1) % field.length;
                    }
                    break;
                case 9:
                    while (field[position].code[0] !== 'U') {
                        position = (position + 1) % field.length;
                    }
                    break;
                case 10:
                    position = position - 3;
                    if (position < 0) {
                        position += field.length;
                    }
                    break;
            }      
        }

        field[position].landed();
        moves += 1;
    }
    field.sort((a:S, b:S) => b.counter - a.counter);
    for (let s of field) {
        console.info(s.code, '\t', s.counter / moves);
    }

    return `${field[0].fullnum()}${field[1].fullnum()}${field[2].fullnum()}`;
}

console.log(simulate(2, 4));
