package process

import models.{SCA, Kingdom, Principality, Barony, Award, AwardName, Champion, Gender}
import Award._
import Gender._

// Second pass at program configuration. After spending a *long* time doing this
// using XML (the original Config class), which results in a very messy class
// and a hard-to-maintain config file, I decided to change tack and instead do
// it in code. There is little good reason to do otherwise -- indeed, sbt
// suggests an idiom of doing it this way.
//
// So this object is basically one gigantic declaration of the way the world
// works. It is invoking a whole lot of case classes that are declared in the
// various models, and setting up the static tables of what things look like.
object Config2 {
  val Society =
    SCA("SCA", 
      isDefault = true, 
      
      // Society-wide awards and concepts
      awards = Seq(
          Award("Queen's Guard"),
          Award("Royal Cypher"),
          Award("King's Cypher"),
          Award("Queen's Cypher"),
          Award("Prince's Cypher"),
          
          Award("Herald Extraordinaire"),
          
          Award("Award of Arms", "Award of Arm", "AOA"),
          
          Award("Court Barony", synonyms = Seq(
            AwardName("Court Baroness", Female),
            AwardName("Court Baron", Male),
            AwardName("Baroness of the Court", Female),
            AwardName("Baron of the Court", Male)
          )),
          Award("Grant of Arms", "Grand of Arms", "GOA"),
          
          Award("White Scarf", "Defender of the White Scarf"),
          
          Award("Patent of Arms", "Patent"),
          Award("Knight", "Chivalry (knight)", "Order of Chivalry (Knight)"),
          // Yes, sometimes the choice didn't get recorded:
          Award("Chivalry", "Member of the Chivalry"),
          Award("Laurel", synonyms = Seq(
            AwardName("Mistress of the Laurel", Female),
            AwardName("Master of the Laurel", Male)
          )),
          Award("Master at Arms", "Master of Arms"),
          Award("Pelican", synonyms = Seq(
            AwardName("Mistress of the Pelican", Female),
            AwardName("Master of the Pelican", Male)
          )),
          Award("Rose", synonyms = Seq(AwardName("Lady of the Rose", Female))),
          
          Award("Viscount", gender = Male, synonyms = Seq(AwardName("Viscountess", Female))),
          Award("Count", gender = Male, synonyms = Seq(AwardName("Countess", Female))),
          Award("Duke", gender = Male, synonyms = Seq(AwardName("Duchess", Female)))
      )
      // Each of these is a Seq[AwardInfo], so they don't fit neatly into the above list
      ++ Champion("Archery", "Archer")
      ++ Champion("Arms", "Rattan", "Heavy List", "Tournament", "List")
      ++ Champion("Arts and Sciences", "A&S", "Arts & Sciences")
      ++ Champion("Bardic", "Bard")
      ++ Champion("Horse", "Equestrian")
      ++ Champion("Rapier", "Fence"),
        
      // Kingdoms
      children = Seq(
        Kingdom("East", isDefault = true,
            
          // Eastrealm awards
          awards = Seq(
            // Common non-awards
            Award("Armorer to the Crown"),
            Award("Grandmaster Bowman"),
            Award("Master Bowman"),
            Award("Seamstress to the Crown", gender = Female, synonyms = Seq(
              AwardName("Taylor to the Crown", Male),
              AwardName("Tailor to the Crown", Male),
              AwardName("Seamstress to the Court", Female))),
            Award("Seamstress to the Princess Royal"),
            Award("Toymaker to the Queen"),
            Award("Court Jester"),
            Award("Admiral of the Armies"),
            Award("Shield of Chivalry"),
            Award("Youth Archery Champion"),
            Award("Confirmation of Gentility"),
            
            // Closed Orders
            Award("Guardsman"),
            Award("Fatima"),
            Award("Pheon"),
            
            // Official non-precedential awards from the Crown
            Award("Queen's Honor of Distinction", "Queen's Honour of Distinction", "Queens's Honor of Distinction", "Queen Honor of Distinction"),
            Award("King's Esteem of Merit", "Kings's Esteem of Merit"),
            Award("Tyger of Valor", "King's Tyger of Valor", "Valiant Tyger", "King's Valor"),
            Award("Blue Tyger Legion"),
            Award("Tyger of Foreign Legions", "Foreign Tyger"),
            Award("Golden Tyger", "Golden Lyre"),
            Award("Royal Augmentation of Arms"),
            Award("Kingdom Augmentation of Arms", "(Kingdom) Augmentation of Arms", "Augmentation of Arms"),
            // Yes, the Tyger carries no precedence -- it just has all the prestige ever:
            Award("Tyger of the East"),
            
            // Orders of Honor
            Award("Artemis"),
            Award("Burdened Tyger", "Brudened Tyger", "Budened Tyger", "Burden Tyger"),
            Award("Gawain", "Gawan"),
            Award("Golden Mantle"),
            Award("Queen's Order of Courtesy", "Queen's Courtesy"),
            Award("King's Order of Excellence", "King's Excellence"),
            Award("Silver Rapier"),
            Award("Terpsichore", "Trepsichore"),
            Award("Troubadour", "Troubadours"),
            Award("Tygers Cub", "Tyger Cub"),
            
            // Orders of High Merit
            Award("Golden Rapier"),
            Award("Maunche", "Mauche", "Manche", "Manch"),
            Award("Sagittarius", "Sagitarius"),
            Award("Silver Crescent", "Sliver Crescent"),
            Award("Tygers Combattant", "Tygers Combatant", "Tyger Combatant"),
            
            // Royalty -- not awards, but frequently recorded in the OP
            Award("Queen of the East Kingdom", gender = Female),
            Award("King of the East", gender = Male),
            Award("Crown Prince", gender = Male, synonyms = Seq(
              AwardName("Prince", Male),
              AwardName("Prinz", Male)
            )),
            Award("Crown Princess", gender = Female, synonyms = Seq(
              AwardName("Princess", Female),
              AwardName("Prinzessin", Female)
            )),
            
            // Officers
            Award("Kingdom Marshal of Fence")
          ),
            
          children = Seq(
            // Baronies
            Barony("Ostgardr", awards = Seq(
              Award("Seadog", "Sea Dog"),
              "Seahorse",
              "Augmented Seahorse",
              "Sea Star"
            )),
            
            Barony("Carolingia", awards = Seq(
              "Daystar",
              "Moon",
              "Perseus",
              "Free Griffin"
            )),
            
            Barony("Beyond the Mountain", synonyms = Seq("BBM"), awards = Seq(
              "Holly",
              Award("Sun and Soil", "Sun & Soil"),
              Award("White Oak", "Defender of the White Oak")
            )),
            
            Barony("Bhakail", awards = Seq(
              Award("Flame of Bhakail", "Guardian of the Flame"),
              "Harlequin",
              Award("Master of Horse", "Horse"),
              "Salamander"
            )),
            
            Barony("Bridge", synonyms = Seq("the Bridge"), awards = Seq(
              "Freedom of the Bridge",
              Award("Pillar of the Bridge", "Pillar")
            )),
            
            Barony("Dragonship Haven", awards = Seq(
              "Yale",
              "Hawk's Bell",
              "Keel",
              "Oar",
              Award("St. Martin", "Company of St. Martin")
            )),
            
            Barony("Concordia of the Snows", synonyms = Seq("Concordia"), awards = Seq(
              Award("Friends", "Friend of Concordia"),
              "Pine",
              "Sapphire",
              "Snow Hare"
            )),
            
            Barony("Settmour Swamp", synonyms = Seq("Swamp"), awards = Seq(
              "Bloody Tower",
              "Bronze Tower",
              "Iron Tower",
              "Ivory Tower",
              "Silver Tower"
            )),
            
            Barony("Mountain Freehold"),
            
            Barony("Carillion", awards = Seq(
              "Chime",
              "Fish",
              "Sable Bell",
              "Light of Carillion",
              "Horse's Head",
              "Shrouded Bell"
            )),
            
            Barony("Stonemarche", awards = Seq(
              Award("Furison", "Fusion"),
              "Lamp of Apollo",
              "Millstone"
            )),
            
            Barony("Bergental"),
            
            Barony("An Dubhaigeainn", awards = Seq(
              "Drake's Spur",
              "Silver Feather"
            )),
            
            Barony("Smoking Rocks", awards = Seq(
                "Leviathan",
                "Quadrant",
                "Rock"
            )),
            
            Barony("Iron Bog", awards = Seq(
              "Baroness's Silver Ring",
              "Duck",
              Award("Sable Compass", "Sable Compuss"),
              "Sable Martlet",
              "Silver Compass",
              "Silver Gauntlet",
              "Silver Martlet",
              "Silver Cattail"
            )),
            
            // Tir Mara and its Baronies
            Principality("Tir Mara", children = Seq(
	          Barony("Ruantallan", awards = Seq(
	            Award("Iceburg", "Iceberg")
	          )),
            
              Barony("Havre des Glaces", synonyms = Seq("Havre de Glaces", "Havres des Glaces"), awards = Seq(
                Award("Ordre du Pèlerin", "Ordre du Pelerin"),
                "Chiffre du Baron",
                Award("Order du Lys d'argent", "Ordre du Lys d'Argent"),
                "Ordre du Meritum Martialis",
                "Order de Mai"
              )),
            
              Barony("L'Ile du Dragon Dormant")            
            ))
        )),
          
        Kingdom("AEthelmearc", 
          awards = Seq(
            "Golden Alce",
            "Keystone",
            "Sigil of AEthelmearc",
            "Sycamore"
          ),
        
          children = Seq(
            Barony("BMDL", awards = Seq(
              Award("Gold Comet", "Comet Or"),
              Award("Green Comet", "Comet Vert"),
              "Silver Comet",
              "Iron Comet",
              "Red Comet"
            )),
            
            Barony("Delftwood", awards = Seq(
              "Windmill"
            )),
            
            Barony("Thescorre", awards = Seq(
              "Raven's Feather"
            ))
          )
        ),
        
        Kingdom("Ansteorra"),
        
        Kingdom("An Tir", 
          children = Seq(
            Barony("Adiantum", awards = Seq(
              "Baronial Brownie"
            ))
          )
        ),
        
        Kingdom("Artemisia",
          awards = Seq(
            "Golden Feather of Artemisia",
            "Grace of Artemisia",
            "Griffin's Heart",
            Award("Griffin's Talon", "Gryphon's Talon"),
            "Lady of the Papillon",
            "LUST",
            "Maple Leaf of Artemisia",
            "Pillar of Artemisia"
          ),
          
          children = Seq(
            Barony("Arn Hold", awards = Seq(
              "Moose Combatant of Arn Hold"
            )),
            
            Barony("Loch Salaan", awards = Seq(
              "Bannthegn",
              Award("Crystal of the Salt Wastes", "Crystal of the Salt Waste"),
              Award("Devoted and Unique Company of Keepers", "Devoted & Unique Company of Keepers"),
              "Falcon of Loch Salaan",
              "Flower of Chivalry"
            ))
          )
        ),
        
        Kingdom("Atenveldt",
          awards = Seq(
            "Fleur de Soleil",
            "Light of Atenveldt",
            "Lion of Atenveldt",
            "King's Sigil",
            "Queen's Grace of Atenveldt",
            "Beacon of the Desert",
            "Thegn",
            "Hawk's Lure"
          ),
          
          children = Seq(
            Principality("Sun", awards = Seq(
              "Solar Heart"
            ))
          )
        ),
        
        Kingdom("Atlantia",
          awards = Seq(
            "Golden Dolphin",
            "Pearl",
            "Sea Stag"
          ),
          
          children = Seq(
            Barony("Black Diamond", awards = Seq(
              "Polished Mirror",
              "Silver Chalice"
            )),
            
            Barony("Bright Hills", awards = Seq(
              "Cat's Paw",
              "Silver Scroll",
              "Baronial award of Excellence",
              "Blue Collar"
            )),
            
            Barony("Caer Mear", awards = Seq(
              "La Brise de Mer",
              "Pharos"
            )),
            
            //  Note that Myrkwode was dissolved into Atlantia
            Barony("Myrkwood", awards = Seq(
              "Windblown Leaf"
            )),
            
            Barony("Stierbach", awards = Seq(
              "St. Roche"
            ))
          )
        ),
        
        Kingdom("Caid", awards = Seq(
          "Corde de Guerre of Caid",
          "Dolphin"
        )),
        
        Kingdom("Calontir",
          awards = Seq(
            Award("Golden Calon Swan", "Swan"),
            "Iren Fyrd",
            "Iren Hirth"
          ),
          
          children = Seq(
              Barony("Lonely Tower", awards = Seq(
                  "Lonely Tower"
              ))
          )
        ),
        
        Kingdom("Drachenwald",
          awards = Seq(
            "Principality Company of Sojourners",
            Award("Princess's Order of Courtesy", "Princess's Courtesy"),
            "Dragons Pride",
            "Lady of the Edelweiss",
            "Lindquistrings",
            "Prince's Companions of Albion"
          )
        ),
        
        Kingdom("Ealdormere",
          awards = Seq(
            "Bee",
            "OHS",
            "Award of Tangwystyl's Favor",
            "Orion"
          )
        ),
        
        Kingdom("Gleann Abhann"),
        
        Kingdom("Lochac"),
        
        Kingdom("Meridies"),
        
        Kingdom("Middle",
          awards = Seq(
            "Dragon's Heart",
            "Gaping Wound",
            "Purple Fret",
            "Silver Oak",
            "Willow",
            "Queen's Favor"
          ),
          
          children = Seq(
            Barony("Middle Marches", awards = Seq(
              "Watchful Tower"
            )),
            
            Barony("North Woods", awards = Seq(
              "White Wolf"
            ))
          )
        ),
        
        Kingdom("Northshield",
          awards = Seq(
            "Aegis"
          ),
          
          children = Seq(
            Barony("Nordskogen", awards = Seq(
              "Baronial Broom",
              "Baton Gules",
              "Heavy Cross",
              "Honor of the Heliotrope"
            ))
          )
        ),
        
        Kingdom("Outlands",
          awards = Seq(
            "Flower",
            "Golden Reflection",
            "Stag",
            "Stag's Blood",
            "Stag's Heart",
            "Stag's Tynes",
			"Trefoil"
          ),
          
          children = Seq(
            Barony("al-Barran", awards = Seq(
              "Russian Thistle of al-Barran",
              "Scorpion of al-Barran"
            )),
            
            Barony("Caerthe", awards = Seq(
              "Aspen of Caerthe",
              "Gilded Leaf"
            )),
            
            Barony("Dragonsspine", awards = Seq(
              "Scales of Dragonsspine"
            )),
            
            Barony("Ered Sul", awards = Seq(
              "Gilded Heart of Ered Sul"
            )),
            
            Barony("Irel", awards = Seq(
              "Cordon Royale"
            ))
          )
        ),
        
        Kingdom("Trimaris",
          awards = Seq(
            "Argent Estoile",
            "Argent Morningstar",
            "Argent Scales",
            "Bards Laureate",
            "Emerald Seas",
            "Herald's Tressure",
            "Silver Trident",
            "Trade Winds"
          ),
          
          children = Seq(
            Barony("Wyvernwood", awards = Seq(
              "Wyvern's Claw",
              "Wyvern's Scale"
            ))
          )
        ),
        
        Kingdom("West",
          awards = Seq(
            "Commendabilis",
            "Defender of the West",
            "Knight Bannerette",
            "Leaf of Merit",
            "Old Battered Helm",
            "Pied d'Argent",
            "Queen's Grace",
            "Silver Mantle",
            "Silver Molet",
            "Valor",
            "Wooden Spoon"
          ),
          
          children = Seq(
            Principality("Cynagua", awards = Seq(
              "Friendly Castle",
              "Hearthstone",
              "La Courtesia",
              "Princess's Token",
              "Ruxton"
            )),
            
            Principality("Mists", awards = Seq(
              "Corolla Muralis",
              "Corolla Nebularum",
              "Princess's Grace",
              "Rose Leaf"
            ))
          )
        )
    ))
    
  def getWorld = Society
}