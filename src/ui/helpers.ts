export const capitalizeWord = (word: string): string =>
  word[0].toUpperCase() + word.slice(1);

export const capitalize = (s: string): string =>
  s.split(" ").map(capitalizeWord).join(" ");
