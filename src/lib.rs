use std::fmt;
use unicode_normalization::UnicodeNormalization;

#[derive(Debug)]
pub enum Error {
    InvalidStem,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for Error {}

#[derive(Copy, Clone, Debug, Default)]
pub enum Number {
    #[default]
    Singular,
    Dual,
    Plural,
}

impl Number {
    pub const fn all() -> [Number; 3] {
        [Number::Singular, Number::Dual, Number::Plural]
    }
}

pub enum Case {
    Nominative,
    Genitive,
    Dative,
    Accusative,
    Vocative,
}

pub enum Declension {
    E,
    Os,
    On,
}

pub enum Gender {
    Masculine,
    Feminine,
    Neuter,
}

pub struct DeclensionProduct {
    pub number: Number,
    pub case: Case,
}

pub trait Noun {
    fn stem(&self) -> &str;
    fn gender(&self) -> Gender;
    fn declined(self, dec: DeclensionProduct) -> String;
}

#[derive(Default)]
pub enum Mood {
    #[default]
    Indicative,
    Imperative,
    Subjunctive,
    Optative,
}

#[derive(Default)]
pub enum Voice {
    #[default]
    Active,
    Middle,
    Passive,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum Person {
    #[default]
    First,
    Second,
    Third,
}

impl Person {
    pub const fn all() -> [Person; 3] {
        [Person::First, Person::Second, Person::Third]
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub enum Tense {
    #[default]
    Present,
    Imperfect,
    Future,
}

impl Tense {
    pub const fn all() -> [Tense; 3] {
        [Tense::Present, Tense::Imperfect, Tense::Future]
    }
}

#[derive(Default)]
pub struct ConjugationProduct {
    pub mood: Mood,
    pub voice: Voice,
    pub person: Person,
    pub number: Number,
    pub tense: Tense,
}

pub trait Verb {
    fn conjugated(self, con: ConjugationProduct) -> String;
}

#[derive(Debug, Clone)]
pub enum StemHead {
    Consonant(String),
    Vowel(String),
}

impl StemHead {
    pub fn as_string(self) -> String {
        match self {
            StemHead::Consonant(s) => s,
            StemHead::Vowel(s) => s,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Stem {
    head: StemHead,
    tail: String,
}

fn is_vowel(c: char) -> bool {
    match c {
        'α' | 'ε' | 'ι' | 'η' | 'ο' | 'ω' | 'υ' => true,
        _ => false,
    }
}

impl Stem {
    pub fn new(stem: String) -> Result<Self, Error> {
        let first = stem.chars().next().ok_or(Error::InvalidStem)?;
        let mut dec = first.nfd();
        let head = match dec.any(is_vowel) {
            true => StemHead::Vowel(first.to_string()),
            false => StemHead::Consonant(first.to_string()),
        };
        let tail: String = stem.chars().skip(1).collect();
        Ok(Stem { head, tail })
    }

    pub fn to_string(self) -> String {
        format!("{}{}", self.head.as_string(), self.tail)
    }

    pub fn future_from_present(present: Stem) -> Result<Self, Error> {
        let len = present.tail.len();
        let last = present.tail.chars().last().ok_or(Error::InvalidStem)?;
        let precedent: String = present
            .tail
            .chars()
            .take(len.checked_sub(4).unwrap_or(0))
            .collect();
        let future = match last {
            'κ' | 'χ' | 'γ' => precedent + "ξ",
            'π' | 'β' | 'φ' => precedent + "ψ",
            'τ' | 'δ' | 'θ' => precedent + "σ",
            'α' | 'ε' | 'ι' | 'η' | 'ο' | 'ω' | 'υ' => precedent + "σ",
            _ => format!("{}{}{}", precedent, last, "σ"),
        };
        Ok(Stem {
            head: present.head,
            tail: future,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ThematicVerb {
    present: Stem,
    future: Stem,
    augment: String,
}

impl ThematicVerb {
    pub fn new(present: Stem) -> Result<Self, Error> {
        Ok(Self {
            present: present.clone(),
            future: Stem::future_from_present(present.clone())?,
            augment: ThematicVerb::augment(present.head)
        })
    }
    pub fn augment(head: StemHead) -> String {
        match head {
            StemHead::Consonant(c) => "ε".to_string() + &c,
            StemHead::Vowel(e) => match e.as_str() {
                "α" | "ε" => "η",
                "ι" => "ῑ",
                "ο" => "ω",
                "υ" => "ῡ",
                "αι" | "ᾳ" => "ῃ",
                "οι" => "ῳ",
                _ => panic!()
            }.to_string()
        }
    }
    pub fn ending(con: ConjugationProduct) -> String {
        match con.mood {
            Mood::Indicative => match con.voice {
                Voice::Active => match con.tense {
                    Tense::Present | Tense::Future => match con.number {
                        Number::Singular => match con.person {
                            Person::First => "ω",
                            Person::Second => "εις",
                            Person::Third => "ει",
                        },
                        Number::Dual => todo!(),
                        Number::Plural => match con.person {
                            Person::First => "ομεν",
                            Person::Second => "ετε",
                            Person::Third => "ουσι",
                        },
                    },
                    Tense::Imperfect => match con.number {
                        Number::Singular => match con.person {
                            Person::First => "ον",
                            Person::Second => "ες",
                            Person::Third => "ε",
                        },
                        Number::Dual => todo!(),
                        Number::Plural => match con.person {
                            Person::First => "ομεν",
                            Person::Second => "ετε",
                            Person::Third => "ον",
                        },
                    },
                },
                _ => todo!(),
            },

            _ => todo!(),
        }
        .to_string()
    }
}

impl Verb for ThematicVerb {
    fn conjugated(self, con: ConjugationProduct) -> String {
        (match con.tense {
            Tense::Present => self.present.to_string(),
            Tense::Imperfect => self.augment + &self.present.tail,
            Tense::Future => self.future.to_string(),
        }) + &ThematicVerb::ending(con)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verb_stems() -> Result<(), Error> {
        let ago = ThematicVerb::new(Stem::new("αγ".to_string())?)?;
        let grapho = ThematicVerb::new(Stem::new("γραφ".to_string())?)?;
        let peitho = ThematicVerb::new(Stem::new("πειθ".to_string())?)?;
        let pisteuo = ThematicVerb::new(Stem::new("πιστευ".to_string())?)?;

        assert_eq!(ago.present.to_string(), "αγ".to_string());
        assert_eq!(ago.future.to_string(), "αξ".to_string());
        assert_eq!(grapho.present.to_string(), "γραφ".to_string());
        assert_eq!(grapho.future.to_string(), "γραψ".to_string());
        assert_eq!(peitho.present.to_string(), "πειθ".to_string());
        assert_eq!(peitho.future.to_string(), "πεισ".to_string());
        assert_eq!(pisteuo.present.to_string(), "πιστευ".to_string());
        assert_eq!(pisteuo.future.to_string(), "πιστευσ".to_string());
        Ok(())
    }
}
