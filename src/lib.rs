use std::{
    borrow::{Borrow, BorrowMut},
    cmp::Ordering,
    collections::{BTreeSet, HashMap, HashSet},
    fmt,
    ops::{Add, AddAssign, Sub, SubAssign},
    ops::{Deref, DerefMut},
};

use once_cell::sync::Lazy;

static CHAR_ALIASES: Lazy<HashMap<char, char>> = Lazy::new(|| {
    let mut map = HashMap::new();
    const CASE_DIFF: u8 = b'a' - b'A';
    for c in b'A'..=b'Z' {
        map.insert(c as char, (c + CASE_DIFF) as char);
    }
    map.insert('$', 's');
    map.insert('4', 'a');
    map.insert('@', 'a');
    map.insert('!', 'i');
    map.insert('2', 'z');
    map.insert('0', 'o');
    map.insert('3', 'e');
    map.insert('5', 's');
    map.insert('6', 'g');
    map.insert('£', 'e');
    map.insert('€', 'e');
    map.insert('¢', 'c');
    map.insert('¥', 'y');
    map
});

macro_rules! word_set {
    ($name:ident, $($word:literal),*) => {
        static $name: Lazy<HashSet<Word>> = Lazy::new(|| {
            let mut set = HashSet::new();
            for s in &[$($word),*] {
                set.insert(Word::from(*s));
            }
            set
        });
    };
}

word_set!(
    STANDARD_WORDS,
    "ass",
    "asshole",
    "bitch",
    "cock",
    "cunt",
    "fag",
    "faggot",
    "fuck",
    "nigger",
    "pussy",
    "shit",
    "twat",
    "whore"
);
word_set!(ZEALOUS_WORDS, "crap", "damn", "hell", "suck");
word_set!(
    SEX_WORDS,
    "ass",
    "asshole",
    "blowjob",
    "boob",
    "boobjob",
    "breast",
    "clitoris",
    "cock",
    "cunnilingus",
    "cunt",
    "dick",
    "doggystyle",
    "ejaculate",
    "felate",
    "felatio",
    "fetish",
    "foreskin",
    "handjob",
    "labia",
    "masterbate",
    "masterbation",
    "penis",
    "pussy",
    "rimjob",
    "semen",
    "sex",
    "tits",
    "titties",
    "titty",
    "twat",
    "vagina",
    "vulva"
);
word_set!(SLUR_WORDS, "fag", "faggot", "gook", "nigger", "spic", "spick");

#[derive(Clone, PartialEq, Eq, Hash, Default)]
pub struct Word(pub String);

impl fmt::Debug for Word {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <String as fmt::Debug>::fmt(&self.0, f)
    }
}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <String as fmt::Display>::fmt(&self.0, f)
    }
}

impl<S> From<S> for Word
where
    S: Into<String>,
{
    fn from(s: S) -> Self {
        Word(s.into())
    }
}

impl Deref for Word {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Word {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl AsRef<String> for Word {
    fn as_ref(&self) -> &String {
        &self.0
    }
}

impl AsMut<String> for Word {
    fn as_mut(&mut self) -> &mut String {
        &mut self.0
    }
}

impl Borrow<String> for Word {
    fn borrow(&self) -> &String {
        &self.0
    }
}

impl BorrowMut<String> for Word {
    fn borrow_mut(&mut self) -> &mut String {
        &mut self.0
    }
}

impl PartialOrd for Word {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0
            .len()
            .partial_cmp(&other.0.len())
            .map(Ordering::reverse)
    }
}

impl Ord for Word {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.len().cmp(&other.0.len()).reverse()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Censor {
    Standard,
    Sex,
    Zealous,
    Slurs,
    Custom(HashSet<Word>),
}

pub use Censor::*;

impl Default for Censor {
    fn default() -> Self {
        Standard
    }
}

impl Censor {
    pub fn bad_chars(&self, text: &str) -> BTreeSet<usize> {
        let lowercase = text.to_lowercase();
        // Check just alphanumeric
        let (alphanum_only, alphanum_map) = remove_non_alphanumeric(&lowercase);
        let bad_alphanum_chars = self._bad_chars(&alphanum_only, &alphanum_map);
        // Check aliased and without whitespace
        let (aliased, aliased_map) = remove_whitespace(&alias(&lowercase));
        let bad_aliased_chars = self._bad_chars(&aliased, &aliased_map);
        // Union sets
        bad_alphanum_chars
            .union(&bad_aliased_chars)
            .copied()
            .collect()
    }
    pub fn check(&self, text: &str) -> bool {
        !self.bad_chars(text).is_empty()
    }
    pub fn replace(&self, text: &str, replacement_char: char) -> String {
        let bad_chars = self.bad_chars(text);
        text.chars()
            .enumerate()
            .map(|(i, c)| {
                if bad_chars.contains(&i) {
                    replacement_char
                } else {
                    c
                }
            })
            .collect()
    }
    fn _bad_chars(&self, text: &str, map: &HashMap<usize, usize>) -> BTreeSet<usize> {
        let mut set = BTreeSet::new();
        for word in self.set() {
            for (i, _) in text.match_indices(word.as_ref()) {
                for j in 0..word.len() {
                    let k = i + j;
                    if let Some(k) = map.get(&k) {
                        set.insert(*k);
                    }
                }
            }
        }
        set
    }
    pub fn set(&self) -> &HashSet<Word> {
        match self {
            Standard => &*STANDARD_WORDS,
            Zealous => &*ZEALOUS_WORDS,
            Sex => &*SEX_WORDS,
            Slurs => &*SLUR_WORDS,
            Custom(words) => words,
        }
    }
    pub fn list(&self) -> std::collections::hash_set::Iter<Word> {
        self.set().iter()
    }
    pub fn find(&self, word: &str) -> Option<&str> {
        let word = alias(word);
        self.set().get(&word).map(|w| w.as_str())
    }
    pub fn contains(&self, word: &str) -> bool {
        self.find(word).is_some()
    }
}

impl AddAssign for Censor {
    fn add_assign(&mut self, other: Self) {
        *self = Censor::Custom(self.set().union(other.set()).cloned().collect());
    }
}

impl<S> AddAssign<S> for Censor
where
    S: Into<Word>,
{
    fn add_assign(&mut self, other: S) {
        *self = Censor::Custom(self.list().cloned().chain(Some(other.into())).collect());
    }
}

impl SubAssign for Censor {
    fn sub_assign(&mut self, other: Self) {
        *self = Censor::Custom(self.set().difference(other.set()).cloned().collect());
    }
}

impl<S> SubAssign<S> for Censor
where
    S: Into<Word>,
{
    fn sub_assign(&mut self, other: S) {
        let other = other.into();
        *self = Censor::Custom(self.list().filter(|&s| s != &other).cloned().collect());
    }
}

impl Add for Censor {
    type Output = Censor;
    fn add(mut self, other: Self) -> Self::Output {
        self += other;
        self
    }
}

impl<S> Add<S> for Censor
where
    S: Into<Word>,
{
    type Output = Censor;
    fn add(mut self, other: S) -> Self::Output {
        self += other;
        self
    }
}

impl Sub for Censor {
    type Output = Censor;
    fn sub(mut self, other: Self) -> Self::Output {
        self -= other;
        self
    }
}

impl<S> Sub<S> for Censor
where
    S: Into<Word>,
{
    type Output = Censor;
    fn sub(mut self, other: S) -> Self::Output {
        self -= other;
        self
    }
}

fn alias(text: &str) -> String {
    text.chars()
        .map(|c| CHAR_ALIASES.get(&c).copied().unwrap_or(c))
        .collect()
}

fn remove_whitespace(text: &str) -> (String, HashMap<usize, usize>) {
    let mut output = String::new();
    let mut map = HashMap::new();
    for (i, (j, c)) in text
        .chars()
        .enumerate()
        .filter(|(_, c)| !c.is_whitespace())
        .enumerate()
    {
        output.push(c);
        map.insert(i, j);
    }
    (output, map)
}

fn remove_non_alphanumeric(text: &str) -> (String, HashMap<usize, usize>) {
    let mut output = String::new();
    let mut map = HashMap::new();
    for (i, (j, c)) in text
        .chars()
        .enumerate()
        .filter(|(_, c)| c.is_alphanumeric())
        .enumerate()
    {
        output.push(c);
        map.insert(i, j);
    }
    (output, map)
}
