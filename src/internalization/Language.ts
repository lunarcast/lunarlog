export type CompoundName = "rule" | "node";

export interface Language {
  adjectives: string[];
  rule: string;
  node: string;
  nameCompound: Record<CompoundName, string>;
  createCompound: Record<CompoundName, string>;
  readTheTutorial: [string, string];
  checkItOutOnGithhub: [string, string];
  nameTaken: (name: string) => string;
  noRulesFound: string;
  argumentCount: string;
  cannotCreateNodeWithoutRule: string;
  cannotUseNodeWihtoutRule: string;
  cannotQueryWithoutRule: string;
  tabs: Record<
    "info" | "createRule" | "createNode" | "editQuery" | "rules" | "useNode",
    string
  >;
  language: string;
  noNodes: string;
  myCompound: (name: string) => string;
  noSolutions: string;
  infiniteRecursion: string;
}

export const english: Language = {
  adjectives: [
    "interesting",
    "intriguing",
    "aweomse",
    "favorite",
    "cool",
    "important",
    "special",
    "unique",
    "original",
    "creative",
  ],
  rule: "rule",
  node: "node",
  nameCompound: {
    node: "Node name",
    rule: "Rule name",
  },
  createCompound: {
    rule: "Create rule",
    node: "Create node",
  },
  argumentCount: "Argument count",
  readTheTutorial: ["Read the", "tutorial"],
  checkItOutOnGithhub: ["Check it out on", "github"],
  nameTaken: (name) => `Name "${name}" is already taken`,
  noRulesFound: "No rules found",
  cannotCreateNodeWithoutRule:
    "Cannot create nodes without creating a rule first",
  cannotUseNodeWihtoutRule: "Cannot use nodes without creating a rule first",
  cannotQueryWithoutRule: "Cannot query an empty project",
  tabs: {
    info: "Info",
    createNode: "Create node",
    createRule: "Create rule",
    editQuery: "Edit query",
    rules: "Rules",
    useNode: "Use node",
  },
  noNodes: "You haven't created any node yet!",
  myCompound: (name) => `My${name}`,
  language: "Language: english",
  noSolutions: "No solutions found",
  infiniteRecursion:
    "Couldn't solve constraint. Possibly infinite recursion encountered",
};

export const dutch: Language = {
  adjectives: [
    "interessant",
    "fascinerend",
    "geweldig",
    "favoriet",
    "cool",
    "belangrijk",
    "speciaal",
    "uniek",
    "origineel",
    "creatief",
  ],
  rule: "regel",
  node: "node",
  nameCompound: {
    node: "Node naam",
    rule: "Regel naam",
  },
  createCompound: {
    rule: "Creer regel",
    node: "Creer node",
  },
  argumentCount: "Aantal argumenten",
  readTheTutorial: ["Lees de", "tutorial"],
  checkItOutOnGithhub: ["Bekijk op", "github"],
  nameTaken: (name) => `Naam "${name}" is al in gebruik`,
  noRulesFound: "Geen regels gevonden",
  cannotCreateNodeWithoutRule:
    "Kan geen node creeren zonder eerst een regel te maken",
  cannotUseNodeWihtoutRule:
    "kan geen node gebruiken zonder eerst een regel te maken",
  cannotQueryWithoutRule: "kan niet een leeg project queryen",
  tabs: {
    info: "Info",
    createNode: "Maak node",
    createRule: "Maak regel",
    editQuery: "Bewerk query",
    rules: "Regels",
    useNode: "Gebruik node",
  },
  noNodes: "Je hebt nog geen node gemaakt!",
  myCompound: (name) => `Mijn${name}`,
  language: "Taal: Nederlands",
  noSolutions: "geen oplossingen gevonden",
  infiniteRecursion:
    "Kon restrictie niet oplossen, mogenlijk oneindige recursie",
};
