class OutfitSystem {
    constructor() {
        this.outfitTypes = {
            noble: 'noble',
            common: 'common'
        };
    }

    canSwapOutfits(gameState, character1, character2) {
        // Проверяем, что персонажи находятся в одной локации
        return gameState.stats[character1].location === gameState.stats[character2].location;
    }

    swapOutfits(gameState, character1, character2) {
        if (!this.canSwapOutfits(gameState, character1, character2)) {
            return {
                success: false,
                message: "Персонажи должны находиться в одной локации для смены одежды"
            };
        }

        // Меняем одежду местами
        const outfit1 = gameState.stats[character1].outfit;
        const outfit2 = gameState.stats[character2].outfit;

        gameState.stats[character1].outfit = outfit2;
        gameState.stats[character2].outfit = outfit1;

        return {
            success: true,
            message: `${character1} и ${character2} поменялись одеждой`,
            newOutfits: {
                [character1]: outfit2,
                [character2]: outfit1
            }
        };
    }

    getOutfitForCharacter(gameState, character) {
        return gameState.stats[character]?.outfit || this.outfitTypes.noble;
    }

    setOutfitForCharacter(gameState, character, outfit) {
        if (!this.outfitTypes[outfit]) {
            throw new Error(`Invalid outfit type: ${outfit}`);
        }
        
        gameState.stats[character].outfit = outfit;
        return this.getOutfitForCharacter(gameState, character);
    }

    isWearing(gameState, character, outfit) {
        return this.getOutfitForCharacter(gameState, character) === outfit;
    }

    getOutfitDescription(outfit) {
        const descriptions = {
            noble: 'парадное платье',
            common: 'простая одежда'
        };
        return descriptions[outfit] || outfit;
    }
}

module.exports = OutfitSystem;