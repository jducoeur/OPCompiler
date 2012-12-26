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
          
          // Yes, there exists exactly one Court Count:
          Award("Court Count"),
          
          Award("Viscount", gender = Male, synonyms = Seq(AwardName("Viscountess", Female))),
          Award("Count", gender = Male, synonyms = Seq(
            AwardName("Countess", Female),
            AwardName("Jarl", Male))),
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
            Award("Seamstress to the Princess Royal", gender = Female, synonyms = Seq(
              AwardName("Seamstress to the Princess Royale", Female)
            )),
            Award("Embroideress to the Crown"),
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
            Award(
              "Queen's Honor of Distinction", 
              "Queen's Honour of Distinction", 
              "Queens's Honor of Distinction", 
              "Queen Honor of Distinction",
              "Queen's Distinction"),
            Award("King's Esteem of Merit", "Kings's Esteem of Merit", "KEM"),
            Award("Tyger of Valor", "King's Tyger of Valor", "Valiant Tyger", "King's Valor"),
            Award("Blue Tyger Legion"),
            Award("Tyger of Foreign Legions", "Foreign Tyger"),
            Award("Golden Lyre", "Golden Tyger"),
            Award("Royal Augmentation of Arms", "Royal Augmentation"),
            Award("Kingdom Augmentation of Arms", "(Kingdom) Augmentation of Arms", "Augmentation of Arms", "East Kingdom Augmentation of Arms", "Kingdom Augmentation"),
            // Yes, the Tyger carries no precedence -- it just has all the prestige ever:
            Award("Tyger of the East"),
            
            // Orders of Honor
            Award("Artemis"),
            Award("Burdened Tyger", "Brudened Tyger", "Budened Tyger", "Burden Tyger"),
            Award("Gawain", "Gawan"),
            Award("Golden Mantle"),
            Award("Queen's Order of Courtesy", "Queen's Courtesy"),
            Award("King's Order of Excellence", "King's Excellence", "KOE"),
            Award("Silver Rapier"),
            Award("Terpsichore", "Trepsichore"),
            Award("Troubadour", "Troubadours", "Troubadors", "Troubador"),
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
              "Sea Star",
              Award("Silver Sea-Lion", "Sea-lion")
            )),
            
            Barony("Carolingia", awards = Seq(
              "Daystar",
              "Moon",
              "Perseus",
              "Free Griffin"
            )),
            
            Barony("Beyond the Mountain", synonyms = Seq("BBM"), awards = Seq(
              Award("Holly", "Order of the Holly"),
              Award("Sun and Soil", "Sun & Soil"),
              Award("White Oak", "Defender of the White Oak")
            )),
            
            Barony("Bhakail", awards = Seq(
              Award("Flame of Bhakail", "Guardian of the Flame"),
              "Harlequin",
              Award("Master of Horse", "Horse"),
              "Salamander",
              Award("Bhakail Order of Fence", "Fence of Bhakail"),
              "Salamander's Tear"
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
              Award("St. Martin", "Company of St. Martin", "St Martin"),
              "Worshipful Company of Artificers"
            )),
            
            Barony("Concordia of the Snows", synonyms = Seq("Concordia"), awards = Seq(
              Award("Friends", "Friend of Concordia"),
              "Pine",
              "Sapphire",
              "Snow Hare",
              "Ram's Horn",
              "Silver Snowflake",
              "Feather"
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
              "Shrouded Bell",
              "Unbalanced"
            )),
            
            Barony("Stonemarche", awards = Seq(
              Award("Furison", "Fusion"),
              Award("Lamp of Apollo", "Keeper of the Lamp"),
              "Millstone",
              "Keeper of the Flames"
            )),
            
            Barony("Bergental", awards = Seq(
              "Hourglass",
              "Sable Bear",
              "Fountain of Bergental"
            )),
            
            Barony("An Dubhaigeainn", awards = Seq(
              "Drake's Spur",
              "Roasted Duck",
              "Silver Feather",
              "Silvanus Doke"
            )),
            
            Barony("Smoking Rocks", awards = Seq(
                "Leviathan",
                "Quadrant",
                "Rock",
                "Lodestone",
                "Sheriff of the Smoking Rocks"
            )),
            
            Barony("Iron Bog", awards = Seq(
              "Baroness's Silver Ring",
              "Duck",
              "Iron Punk",
              Award("Sable Compass", "Sable Compuss"),
              "Sable Gauntlet",
              "Sable Martlet",
              Award("Silver Compass", "Silver Compuss"),
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
                Award("Pèlerin", "Ordre du Pèlerin", "Ordre du Pelerin"),
                Award("Chiffre du Baron", "Order Chiffre du Baron"),
                Award("Lys d'Argent", "Order du Lys d'argent", "Ordre du Lys d'Argent", "Order de Lys d'Argent"),
                Award("Meriti Martialis", "Ordre du Meritum Martialis", "Ordo Meriti Martialis", "Order Meriti Martialis"),
                Award("Ordre de Mai", "Order de Mai"),
                Award("Rose des Glaces", "Ordre de la Rose des Glaces", "Ordre de la Rose de Glaces")
              )),
            
              Barony("L'Ile du Dragon Dormant")            
            ))
        )),
          
        Kingdom("AEthelmearc", 
          awards = Seq(
            "Cornelian",
            Award("Garnet", "Lady of the Garnet"),
            "Golden Alce",
            "Golden Stirrup",
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
              "Red Comet",
              "Blue Comet",
              "Peacock"
            )),
            
            Barony("Delftwood", awards = Seq(
              "Windmill"
            )),
            
            Barony("Thescorre", awards = Seq(
              "Raven's Feather"
            ))
          )
        ),
        
        Kingdom("Ansteorra", awards = Seq(
          Award("Compass Rose", "CRA", "ACR"),
          "Iris of Merit",
          Award("Queen's Glove", "Queen's Glove of Ansteorra"),
          "King's Gauntlet",
          "Lion of Ansteorra",
          "Sable Comet",
          "Sable Crane",
          "Sable Falcon",
          "Sable Thistle",
          "Star of Merit"
        ),
        
          children = Seq(
            Barony("Bonwicke", awards = Seq(
              Award("Western Cross of Bonwicke", "OWCB")
            )),
            
            Barony("Bryn Gwlad", awards = Seq(
              "Halberd"
            )),
            
            Barony("Elfsea", awards = Seq(
              "Azure Keystone",
              "Eastern Tower",
              "Portcullis"
            )),
            
            Barony("Namron", awards = Seq(
              "Heart of the Sable Storm"
            )),
            
            Barony("Steppes", awards = Seq(
              "Acorn",
              "Oak"
            ))
          )
        ),
        
        Kingdom("An Tir", 
          awards = Seq(
            "Forget-Me-Not",
            "Golden Unicorn",
            Award("Goutte de Sang", "Goute de Sang"),
            "Jambe de Lion"
          ),
          
          children = Seq(
            Barony("Adiantum", awards = Seq(
              "Baronial Brownie"
            )),
            
            Principality("Avacal", awards = Seq(
              "Elder of Avacal"
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
            Award("Maple Leaf of Artemisia", "Maple Leaf"),
            "Pillar of Artemisia",
            "King's Council"
          ),
          
          children = Seq(
            Barony("Arn Hold", awards = Seq(
              "Moose Combatant of Arn Hold",
              "Tripsichore's Mouffle",
              "S.P.U.D."
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
            "Desert Flower",
            "Fleur de Soleil",
            "Light of Atenveldt",
            "Lion of Atenveldt",
            "King's Sigil",
            "Queen's Grace of Atenveldt",
            "Beacon of the Desert",
            "Thegn",
            "Hawk's Lure",
            "Radiant Rose"
          ),
          
          children = Seq(
            Barony("Atenveldt", awards = Seq(
              "Palm of Barony Atenveldt",
              "Solar Phoenix"
            )),
            
            Principality("Sun", awards = Seq(
              "Esprit de Soleil",
              "Solar Heart"
            )),
            
            Barony("Sun Dragon", awards = Seq(
              "Dragon's Scale",
              "Honor of Sun Dragon",
              "Rainbows Gold"
            ))
          )
        ),
        
        Kingdom("Atlantia",
          awards = Seq(
            "Golden Dolphin",
            "Pearl",
            "Sea Stag",
            "Fountain",
            "Opal",
            "Krakken",
            "Sable Blade",
            "Shark's Tooth",
            "King's Award of Excellence",
            "Queen's Award of Courtesy",
            "Undine",
            "Yew Bow"
          ),
          
          children = Seq(
            Barony("Black Diamond", awards = Seq(
              "Fettered Crane",
              "Polished Mirror",
              "Silver Chalice",
              "Silver Crocus"
            )),
            
            Barony("Bordermarch", awards = Seq(
              "Silent Trumpet"
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
            
            Barony("Highland Foorde", awards = Seq(
              "Mountain Hawk"
            )),
            
            Barony("Lochmere", awards = Seq(
              "Blasted Oak",
              "Eagle's Feather"
            )),
            
            //  Note that Myrkwode was dissolved into Atlantia
            Barony("Myrkwood", awards = Seq(
              "Windblown Leaf"
            )),
            
            Barony("Ponto Alto", awards = Seq(
              "Ponte di Ferro"
            )),
            
            Barony("Stierbach", awards = Seq(
              "St. Roche"
            )),
            
            Barony("Windmaster's Hill", awards = Seq(
              "Boreas",
              "St Nicholas",
              "Tempest"
            ))
          )
        ),
        
        Kingdom("Caid", 
          awards = Seq(
            "Corde de Guerre of Caid",
            Award("Dolphin", "Dolphin of Caid"),
            Award("Harp Argent", "Harp Argent of Caid"),
            "Crossed Swords",
            "Gauntlet",
            "Honneur de la Chanson",
            "Lux Caidis",
            "Legion of Courtesy",
            "Signum Regina",
            "Vanguard of Honor"
          ),
        
          children = Seq(
            Barony("Calafia", awards = Seq(
              "Golden Trident",
              "Serpent's Flame"
            )),
            
            Barony("Dreiburgen", awards = Seq(
              "Illuminated Tower",
              "Tower"
            )),
            
            Barony("Starkhafn", awards = Seq(
              "Flame"
            )),
            
            Barony("Western Seas", awards = Seq(
              "Baronial Label",
              "Sable Clarion"
            ))
          )
        
        ),
        
        Kingdom("Calontir",
          awards = Seq(
            Award("Golden Calon Swan", "Swan"),
            "Iren Fera",
            "Iren Fyrd",
            "Iren Hirth",
            "Leather Mallet",
            "Torse"
          ),
          
          children = Seq(
            Barony("Conn", awards = Seq(
              "King's Favor"
            )),
            
            Barony("Lonely Tower", awards = Seq(
              "Lonely Tower"
            )),
            
            Barony("Vatavia", awards = Seq(
              "Green Cord",
              "White Cord"
            ))
          )
        ),
        
        Kingdom("Drachenwald",
          awards = Seq(
            Award("Popular Company of Sojourners", "Principality Company of Sojourners"),
            Award("Princess's Order of Courtesy", "Princess's Courtesy"),
            "Dragons Pride",
            "Dragons Tear",
            Award("Lady of the Edelweiss", "Lady of the Marguerite"),
            Award("Lindquistrings", "Companion des Lindquistrings"),
            "Prince's Companions of Albion",
            "Panache",
            "Sigillum Coronae",
            "Silver Guard"
          )
        ),
        
        Kingdom("Ealdormere",
          awards = Seq(
            "Bee",
            "OHS",
            "Award of Tangwystyl's Favor",
            "Orion",
            "Thorbjorn's Hammer",
            "Wolf's Tooth",
            Award("St. Crispin", "Peregrine"),
            "Golden Otter",
            "Wain"
          ),
          
          children = Seq(
            Barony("Skraeling Althing", awards = Seq(
              "Hare Salient"
            ))
          )
        ),
        
        Kingdom("Gleann Abhann", awards = Seq(
          "Marble Chalice",
          "Onyx Chalice"
        )),
        
        Kingdom("Lochac", awards = Seq(
          "Cross of Lochac",
          "Silver Helm"
        )),
        
        Kingdom("Meridies", 
          awards = Seq(
            "Meridian Cross"
          ),
        
          children = Seq(
            Barony("Hammerhold", awards = Seq(
              "Battered Wrench",
              "Dwarven Hammer",
              "Tempered Steel"
            ))
          )
        ),
        
        Kingdom("Middle",
          awards = Seq(
            "Doe's Grace",
            "Dragon's Barb",
            "Dragon's Heart",
            "Dragon's Teeth",
            "Dragon's Tooth",
            "Evergreen",
            "Gaping Wound",
            "Gold Mace",
            "Purple Fret",
            "Purple Fretty",
            "Queen's Favor",
            "Red Company",
            "Royal Vanguard",
            "Silver Oak",
            "Willow"
          ),
          
          children = Seq(
            Barony("Andelcrag"),
            
            Barony("Middle Marches", awards = Seq(
              "Gilded Reed",
              "Watchful Tower"
            )),
            
            Barony("North Woods", awards = Seq(
              "White Wolf"
            )),
            
            Barony("Rising Waters", awards = Seq(
              "Baroness' Favour",
              "Chalice's Crystal",
              "Golden Chalice",
              Award("Award of Protectors of the Chalice", "OPC"),
              "Spider",
              "Warriors of the Chalice"
            )),
            
            Barony("Rivenstar", awards = Seq(
              "Rivenstar"
            )),
            
            Barony("Tree-Girt-Sea")
          )
        ),
        
        Kingdom("Northshield",
          awards = Seq(
            "Aegis",
            "Northshield Cross"
          ),
          
          children = Seq(
            Barony("Nordskogen", awards = Seq(
              "Baronial Broom",
              "Baton Gules",
              "Heavy Cross",
              Award("Heliotrope", "Honor of the Heliotrope"),
              "Rouge Bend"
            ))
          )
        ),
        
        Kingdom("Outlands",
          awards = Seq(
            "Argent Hart",
            "Flower",
            "Golden Reflection",
            "Leaping Stag",
            "Stag",
            "Stag's Blood",
            "Stag's Heart",
            "Stag's Tynes",
			"Trefoil"
          ),
          
          children = Seq(
            Barony("al-Barran", awards = Seq(
              Award("Russian Thistle", "Russian Thistle of al-Barran"),
              Award("Scorpion", "Scorpion of al-Barran")
            )),
            
            Barony("Caerthe", awards = Seq(
              "Aspen of Caerthe",
              "Gilded Leaf"
            )),
            
            Barony("Dragonsspine", awards = Seq(
              "Dragon's Blood",
              "Scales of Dragonsspine"
            )),
            
            Barony("Ered Sul", awards = Seq(
              "Gilded Heart of Ered Sul"
            )),
            
            Barony("Irel", awards = Seq(
              "Cordon Royale"
            )),
            
            Barony("Tir Ysgithr", awards = Seq(
              "Lamp of Tir Ysgithr"
            ))
          )
        ),
        
        Kingdom("Trimaris",
          awards = Seq(
            "Argent Estoile",
            "Argent Morningstar",
            "Argent Palm",
            "Argent Scales",
            "Bards Laureate",
            "Crown's Gratitude",
            "Emerald Seas",
            "Healer's Lamp",
            "Herald's Tressure",
            "Silver Trident",
            "Trade Winds"
          ),
          
          children = Seq(
            Barony("Darkwater", awards = Seq(
              "Acorn's Glade",
              "Trident Keype"
            )),
            
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
            "King's Huscarls",
            "Knight Bannerette",
            "Leaf of Merit",
            "Muckin' Great Clubbe",
            "Old Battered Helm",
            "Pied d'Argent",
            "Queen's Grace",
            "Silver Mantle",
            "Silver Molet",
            "Valor",
            "Wooden Spoon",
            "Wreath of Chivalry"
          ),
          
          children = Seq(
            Principality("Cynagua", awards = Seq(
              "Black Swan",
              "Cynaguan Guard",
              "Friendly Castle",
              "Hearthstone",
              "La Courtesia",
              "Lord Defender of Cynagua",
              "Princess's Token",
              "Ruxton"
            )),
            
            Barony("Far West", awards = Seq(
              "Attic Helm",
              "Baronial Gallant",
              "Leaping Dolphin",
              "Tempest Tossed Traveller Gules",
              "Tempest Tossed Traveller Argent",
              "Coronet's Appreciation",
              "Ginger Flower",
              "Sea Griffon",
              Award("Burdened Bouget", "Bearer of the Burdoned Bouchet"),
              "Empty Shell"
            )),
            
            Principality("Mists", awards = Seq(
              "Corolla Muralis",
              Award("Corolla Nebularum", "Corolla"),
              "Golden Branch",
              "Princess' Favor",
              "Princess's Grace",
              "Rose Leaf"
            )),
            
            Principality("Oertha", 
              awards = Seq(
                Award("Oerthan Order of Grace", "Oerthan Grace"),
                "Princess's Riband",
                Award("Wandering Wolves", "Wandering Wolves of Oertha")
              ),
              
              children = Seq(
                Barony("Eskalya", awards = Seq(
                  "Silver Bough"
                )),
                  
                Barony("Winter's Gate", awards = Seq(
                  Award("Estoille", "Estoile")
                ))
              )
            )
          )
        )
    ))
    
  def getWorld = Society
}