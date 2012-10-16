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
      ++ Champion("Arms", "Rattan", "Heavy List")
      ++ Champion("Arts and Sciences", "A&S", "Arts & Sciences")
      ++ Champion("Bardic", "Bard")
      ++ Champion("Horse", "Equestrian")
      ++ Champion("Rapier"),
        
      // Kingdoms
      children = Seq(
        Kingdom("East", isDefault = true,
            
          // Eastrealm awards
          awards = Seq(
            // Common non-awards
            Award("Armorer to the Crown"),
            Award("Master Bowman"),
            Award("Seamstress to the Crown", gender = Female, synonyms = Seq(AwardName("Taylor to the Crown", Male))),
            Award("Toymaker to the Queen"),
            Award("Court Jester"),
            Award("Admiral of the Armies"),
            Award("Shield of Chivalry"),
            Award("Youth Archery Champion"),
            
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
            Award("Tygers Cub"),
            
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
              "Seahorse"
            )),
            
            Barony("Carolingia", awards = Seq(
              "Daystar",
              "Moon",
              "Perseus"
            )),
            
            Barony("Beyond the Mountain", synonyms = Seq("BBM"), awards = Seq(
              "Holly",
              Award("Sun and Soil", "Sun & Soil"),
              Award("White Oak", "Defender of the White Oak")
            )),
            
            Barony("Bhakail", awards = Seq(
              "Flame of Bhakail",
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
              "Oar"
            )),
            
            Barony("Concordia of the Snows", synonyms = Seq("Concordia"), awards = Seq(
              "Friend of Concordia",
              "Pine",
              "Sapphire",
              "Snow Hare"
            )),
            
            Barony("Settmour Swamp", synonyms = Seq("Swamp"), awards = Seq(
              "Bloody Tower",
              "Bronze Tower",
              "Iron Tower",
              "Ivory Tower"
            )),
            
            Barony("Mountain Freehold"),
            
            Barony("Carillion", awards = Seq(
              "Chime",
              "Fish",
              "Sable Bell"
            )),
            
            Barony("Stonemarche", awards = Seq(
              "Furison",
              "Lamp of Apollo",
              "Millstone"
            )),
            
            Barony("Bergental"),
            
            Barony("An Dubhaigeainn", awards = Seq(
              "Drake's Spur",
              "Silver Feather"
            )),
            
            Barony("Smoking Rocks"),
            
            Barony("Iron Bog", awards = Seq(
              "Baroness's Silver Ring",
              "Sable Martlet",
              "Silver Compass",
              "Silver Gauntlet",
              "Silver Martlet"
            )),
            
            // Tir Mara and its Baronies
            Principality("Tir Mara", children = Seq(
	          Barony("Ruantallan", awards = Seq(
	            "Iceburg"
	          )),
            
              Barony("Havre des Glaces", synonyms = Seq("Havre de Glaces", "Havres des Glaces"), awards = Seq(
                Award("Ordre du Pèlerin", "Ordre du Pelerin"),
                "Chiffre du Baron",
                "Order du Lys d'argent",
                "Ordre du Meritum Martialis"
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
              "Gold Comet"
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
            "Griffin's Talon",
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
              "Crystal of the Salt Wastes",
              Award("Devoted and Unique Company of Keepers", "Devoted & Unique Company of Keepers"),
              "Falcon of Loch Salaan"
            ))
          )
        ),
        
        Kingdom("Atenveldt",
          awards = Seq(
            "Light of Atenveldt",
            "Lion of Atenveldt",
            "King's Sigil"
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
            "Pearl"
          ),
          
          children = Seq(
            Barony("Bright Hills", awards = Seq(
              "Cat's Paw",
              "Silver Scroll"
            )),
            
            Barony("Caer Mear", awards = Seq(
              "La Brise de Mer",
              "Pharos"
            )),
            
            //  Note that Myrkwode was dissolved into Atlantia
            Barony("Myrkwood", awards = Seq(
              "Windblown Leaf"
            ))
          )
        ),
        
        Kingdom("Caid"),
        
        Kingdom("Calontir",
          awards = Seq(
            Award("Golden Calon Swan", "Swan")
          )
        ),
        
        Kingdom("Drachenwald",
          awards = Seq(
            "Principality Company of Sojourners"
          )
        ),
        
        Kingdom("Ealdormere",
          awards = Seq(
            "Bee",
            "OHS",
            "Award of Tangwystyl's Favor"
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
            "Willow"
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
            "Stag's Heart"
          ),
          
          children = Seq(
            Barony("al-Barran", awards = Seq(
              "Russian Thistle of al-Barran",
              "Scorpion of al-Barran"
            )),
            
            Barony("Caerthe", awards = Seq(
              "Aspen of Caerthe",
              "Gilded Leaf"
            ))
          )
        ),
        
        Kingdom("Trimaris",
          awards = Seq(
            "Argent Morningstar",
            "Bards Laureate"
          )
        ),
        
        Kingdom("West",
          awards = Seq(
            "Defender of the West",
            "Leaf of Merit",
            "Pied d'Argent",
            "Queen's Grace",
            "Silver Molet",
            "Valor"
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
              "Rose Leaf"
            ))
          )
        )
    ))
    
  def getWorld = Society
}