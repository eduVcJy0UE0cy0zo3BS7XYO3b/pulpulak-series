// Модель княжны и помощницы ведьмы
class Princess {
    constructor() {
        this.name = "Пулпулак";
        this.age = 17;
        this.hairColor = "русые";
        this.personality = "веселая и эрудированная";
        this.currentOutfit = "nightgown"; // ночная рубашка
        this.inventory = [];
        this.location = "princess_chamber";
    }

    addItem(item) {
        this.inventory.push(item);
    }

    removeItem(itemId) {
        this.inventory = this.inventory.filter(item => item.id !== itemId);
    }

    changeOutfit(outfit) {
        this.currentOutfit = outfit;
    }

    increaseLoyalty(amount) {
        this.loyalty = Math.min(100, this.loyalty + amount);
    }

    decreaseLoyalty(amount) {
        this.loyalty = Math.max(0, this.loyalty - amount);
    }
}

class WitchHelper {
    constructor() {
        this.name = "Младшая сестра"; // притворяется сестрой
        this.disguise = "sister";
        this.currentOutfit = "common_dress"; // простое платье
        this.magicalItems = ["translation_earrings", "voice_medallion"];
        this.secretsKnown = ['parents_dead', 'magic_items'];
        this.trustLevel = 75; // доверие княжны к ней
    }

    changeOutfit(outfit) {
        this.currentOutfit = outfit;
    }

    canSwitchOutfits() {
        return true; // может меняться одеждой с княжной
    }

    increaseTrust(amount) {
        this.trustLevel = Math.min(100, this.trustLevel + amount);
    }

    decreaseTrust(amount) {
        this.trustLevel = Math.max(0, this.trustLevel - amount);
    }
}

// Доступные наряды
const OUTFITS = {
    nightgown: {
        id: "nightgown",
        name: "Ночная рубашка",
        description: "Простая белая ночная рубашка",
        socialStatus: "princess",
        suitableFor: ["sleep", "private_chambers"]
    },
    princess_dress: {
        id: "princess_dress", 
        name: "Княжеское платье",
        description: "Роскошное платье из дорогих тканей",
        socialStatus: "princess",
        suitableFor: ["court", "formal_events", "public_appearances"]
    },
    common_dress: {
        id: "common_dress",
        name: "Простое платье",
        description: "Обычное платье простолюдинки",
        socialStatus: "commoner", 
        suitableFor: ["village", "work", "hiding"]
    },
    court_dress: {
        id: "court_dress",
        name: "Придворное платье",
        description: "Элегантное платье придворной дамы",
        socialStatus: "court_lady",
        suitableFor: ["court", "formal_events", "noble_areas"]
    }
};

module.exports = { Princess, WitchHelper, OUTFITS };
