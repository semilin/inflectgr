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

#[derive(Debug, Clone)]
pub enum Syllable {
    Short(String),
    Long(String),
}

impl fmt::Display for Syllable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Syllable::Short(s) => s,
                Syllable::Long(s) => s,
            }
        )
    }
}

fn fundamentalize_string(s: String) -> String {
    s.chars().map(fundamental).collect()
}

impl Syllable {
    pub fn fundamentalized(self) -> Self {
        match self {
            Syllable::Short(s) => Syllable::Short(fundamentalize_string(s)),
            Syllable::Long(s) => Syllable::Long(fundamentalize_string(s)),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum AccentKind {
    Acute,
    Grave,
    Circumflex,
}

#[derive(Debug, Clone)]
pub struct WordSeq {
    pub syls: Vec<Syllable>,
    pub accent_kind: AccentKind,
    pub accent_pos: usize,
    pub breathing: Breathing,
}

impl WordSeq {
    pub fn from_verb(stem: Stem, ending: Vec<Syllable>) -> Self {
        let accent_kind = stem.accent;
        let accent_pos = stem.accent_pos.1;
        let breathing = match stem.head {
            StemHead::Vowel(_, b) => b,
            _ => Breathing::default(),
        };
        let mut syls = stem.composite();
        syls.extend(ending);
        WordSeq {
            syls,
            accent_kind,
            accent_pos,
            breathing,
        }
    }
}

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
    First,
    Second,
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum NomSing {
    Os,
    E,
    A,
    Es,
    As,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum GenSing {
    Es,
    As,
    Ou,
}

struct Noun {
    stem: WordSeq,
    nom_sing: NomSing,
    gen_sing: GenSing,
}

#[derive(Default)]
pub enum Mood {
    #[default]
    Indicative,
    Imperative,
    Subjunctive,
    Optative,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum Voice {
    #[default]
    Active,
    Middle,
    Passive,
}

impl Voice {
    pub const fn all() -> [Voice; 3] {
        [Voice::Active, Voice::Middle, Voice::Passive]
    }
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

#[derive(Default)]
pub struct InfinitiveProduct {
    pub voice: Voice,
    pub tense: Tense,
}

pub trait Verb {
    fn conjugated(self, con: ConjugationProduct) -> WordSeq;
    fn infinitive(self, inf: InfinitiveProduct) -> WordSeq;
}

#[derive(Default, Debug, Copy, Clone)]
pub enum Breathing {
    Rough,
    #[default]
    Smooth,
}

#[derive(Debug, Clone)]
pub enum StemHead {
    Consonant(Syllable),
    Vowel(Syllable, Breathing),
}

impl fmt::Display for StemHead {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.clone().syllable())
    }
}

impl StemHead {
    pub fn syllable(self) -> Syllable {
        match self {
            StemHead::Consonant(s) => s,
            StemHead::Vowel(s, _) => s,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum StemHalf {
    Head,
    Tail,
}

#[derive(Debug, Clone)]
pub struct Stem {
    pub head: StemHead,
    pub tail: Vec<Syllable>,
    pub accent: AccentKind,
    pub accent_pos: (StemHalf, usize),
}

impl Stem {
    pub fn composite(self) -> Vec<Syllable> {
        let mut v = vec![self.head.syllable()];
        v.extend(self.tail);
        v
    }
}

fn is_greek_letter(c: char) -> bool {
    matches!(c, 'α'..'ω')
}

fn is_vowel(c: char) -> bool {
    matches!(c, 'α' | 'ε' | 'ι' | 'η' | 'ο' | 'ω' | 'υ')
}

fn has_vowel(c: char) -> bool {
    c.nfd().any(is_vowel)
}

fn fundamental(c: char) -> char {
    c.nfd().next().unwrap()
}

fn is_long(syl: &str) -> bool {
    syl.chars().any(|c| c.nfd().any(|c| matches!(c, 'ω' | 'η')))
        || syl.chars().filter(|c| has_vowel(*c)).count() > 1
}

fn is_syllable_split(a: char, b: char) -> bool {
    match (is_vowel(a), is_vowel(b)) {
        (true, true) => !matches!(
            (a, b),
            ('ε', 'ι') | ('α', 'ι') | ('ε', 'υ') | ('ο', 'υ') | ('ο', 'ι') | ('α', 'υ')
        ),
        (true, false) => true,
        (false, true) => false,
        (false, false) => !matches!((a, b), ('σ', 'τ') | ('σ', 'θ')),
    }
}

fn syllabify_general(s: &str, full: bool) -> Result<Vec<Syllable>, Error> {
    let binding = s.chars().collect::<Vec<char>>();
    let chars: &[char] = binding.as_slice();
    let vowel_prefix = vowel_prefix_len(s);
    let last_vowel = s
        .chars()
        .enumerate()
        .filter(|(_, c)| has_vowel(*c))
        .last()
        .ok_or(Error::InvalidStem)?;
    let splits: Vec<usize> = chars
        .windows(2)
        .filter(|w| w.len() == 2)
        .map(|w| [fundamental(w[0]), fundamental(w[1])])
        .enumerate()
        .filter(|(i, [a, b])| {
            (!full || *i >= vowel_prefix) && *i != last_vowel.0 && is_syllable_split(*a, *b)
        })
        .map(|(i, _)| i)
        .collect();

    let mut syls: Vec<Syllable> = Vec::new();
    let mut ns = String::new();
    for (i, c) in chars.iter().enumerate() {
        ns.push(*c);
        if splits.contains(&i) {
            syls.push(match is_long(&ns) {
                true => Syllable::Long(ns.clone()),
                false => Syllable::Short(ns.clone()),
            });
            ns.clear();
        }
    }
    if !ns.is_empty() {
        let syl = match is_long(&ns) {
            true => Syllable::Long(ns.to_owned()),
            false => Syllable::Short(ns.to_owned()),
        };
        syls.push(syl);
    }

    Ok(syls)
}

fn syllabify(s: &str) -> Result<Vec<Syllable>, Error> {
    syllabify_general(s, false)
}

fn syllabify_word(s: &str) -> Result<Vec<Syllable>, Error> {
    syllabify_general(s, true)
}

fn vowel_prefix_len(s: &str) -> usize {
    s.chars().take_while(|c| has_vowel(*c)).count()
}

fn augmented_head(augment: Augment, head: StemHead) -> StemHead {
    match augment {
        Augment::Syllabic(syl) => StemHead::Vowel(
            match syl {
                Syllable::Short(s) => Syllable::Short(s),
                Syllable::Long(s) => Syllable::Long(s),
            },
            Breathing::Smooth,
        ),
        Augment::Temporal(s, b) => StemHead::Vowel(
            {
                let head_s = head.syllable().to_string();
                let cons: String = head_s.chars().skip(vowel_prefix_len(&head_s)).collect();
                Syllable::Long(s.to_string() + &cons)
            },
            b,
        ),
    }
}

fn short_s(s: &str) -> Syllable {
    Syllable::Short(s.to_string())
}

fn long_s(s: &str) -> Syllable {
    Syllable::Long(s.to_string())
}

fn char_accent(c: char) -> Option<AccentKind> {
    match c {
        '\u{301}' => Some(AccentKind::Acute),
        '`' => Some(AccentKind::Grave),
        '~' => Some(AccentKind::Circumflex),
        _ => None,
    }
}

fn str_accent(s: &str) -> Option<AccentKind> {
    s.chars()
        .filter_map(|c| c.nfd().filter_map(char_accent).next())
        .next()
}

fn char_breathing(c: char) -> Option<Breathing> {
    match c {
        '\u{314}' => Some(Breathing::Rough),
        '\u{313}' => Some(Breathing::Smooth),
        _ => None,
    }
}

fn str_breathing(s: &str) -> Option<Breathing> {
    s.chars()
        .filter_map(|c| c.nfd().filter_map(char_breathing).next())
        .next()
}

impl fmt::Display for Stem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}",
            self.head,
            self.tail.iter().map(|s| s.to_string()).collect::<String>()
        )
    }
}

impl Stem {
    pub fn new(stem: String) -> Result<Self, Error> {
        let syllables = syllabify_word(&stem)?;

        let first = syllables[0].clone().to_string();
        let tail = &syllables[1..];

        let (accent, accent_pos) = if let Some(a) = str_accent(&first) {
            Some((a, (StemHalf::Head, 0)))
        } else {
            tail.iter()
                .enumerate()
                .filter_map(|(i, s)| str_accent(&s.to_string()).map(|a| (a, (StemHalf::Tail, i))))
                .next()
        }
        .unwrap_or((AccentKind::Acute, (StemHalf::Head, 0)));

        let breathing = str_breathing(&first).unwrap_or_default();

        let first_fundamental = syllables[0].to_owned().fundamentalized();
        let tail_fundamental = tail.iter().map(|s| s.clone().fundamentalized()).collect();

        let head = match has_vowel(first.chars().next().ok_or(Error::InvalidStem)?) {
            true => StemHead::Vowel(first_fundamental, breathing),
            false => StemHead::Consonant(first_fundamental),
        };

        Ok(Stem {
            head,
            tail: tail_fundamental,
            accent,
            accent_pos,
        })
    }

    pub fn future_from_present(present: Stem) -> Result<Self, Error> {
        let len = present.tail.len();
        let end_syl = match len {
            0 => present.head.clone().syllable(),
            _ => present.tail.iter().last().unwrap().clone(),
        };
        let original_long = matches!(end_syl, Syllable::Long(_));
        let end = end_syl.to_string();
        let (last_i, last) = end.chars().enumerate().last().ok_or(Error::InvalidStem)?;
        let precedent: String = end.chars().take(last_i).collect();
        let (future, long) = match last {
            'κ' | 'χ' | 'γ' => (precedent + "ξ", true),
            'π' | 'β' | 'φ' => (precedent + "ψ", true),
            'τ' | 'δ' | 'θ' => (precedent + "σ", true),
            'α' | 'ε' | 'ι' | 'η' | 'ο' | 'ω' => (precedent + "σ", false),
            _ => (format!("{precedent}{last}σ"), false),
        };
        match len {
            0 => {
                let syl = match original_long || long {
                    true => Syllable::Long(future),
                    false => Syllable::Short(future),
                };
                let head = match present.head {
                    StemHead::Consonant(_) => StemHead::Consonant(syl),
                    StemHead::Vowel(_, b) => StemHead::Vowel(syl, b),
                };
                Ok(Stem { head, ..present })
            }
            _ => {
                let len = len - 1;
                let mut future_tail: Vec<Syllable> = present.tail.into_iter().take(len).collect();
                future_tail.push(if original_long || long {
                    Syllable::Long(future)
                } else {
                    Syllable::Short(future)
                });
                Ok(Stem {
                    tail: future_tail,
                    ..present
                })
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ThematicVerb {
    present: Stem,
    future: Stem,
    augment: Augment,
}

#[derive(Debug, Clone)]
pub enum Augment {
    Syllabic(Syllable),
    Temporal(Syllable, Breathing),
}

impl ThematicVerb {
    pub fn new(present: Stem) -> Result<Self, Error> {
        Ok(Self {
            present: present.clone(),
            future: Stem::future_from_present(present.to_owned())?,
            augment: ThematicVerb::augment(present.head),
        })
    }
    pub fn augment(head: StemHead) -> Augment {
        match head {
            StemHead::Consonant(_) => Augment::Syllabic(Syllable::Short("ε".to_string())),
            StemHead::Vowel(e, b) => {
                let s = e.to_string();
                let vowel_prefix: String = s
                    .chars()
                    .map(fundamental)
                    .take(vowel_prefix_len(&s))
                    .collect();
                Augment::Temporal(
                    Syllable::Long(
                        match vowel_prefix.as_str() {
                            "α" | "ε" => "η",
                            "ι" => "ῑ",
                            "ο" => "ω",
                            "υ" => "ῡ",
                            "αι" | "ᾳ" => "ῃ",
                            "οι" => "ῳ",
                            _ => panic!("{e} is not a valid starting vowel"),
                        }
                        .to_string(),
                    ),
                    b,
                )
            }
        }
    }
    pub fn ending(con: ConjugationProduct) -> Vec<Syllable> {
        use Syllable as S;
        match con.mood {
            Mood::Indicative => match con.voice {
                Voice::Active => match con.tense {
                    Tense::Present | Tense::Future => match con.number {
                        Number::Singular => vec![S::Long(
                            match con.person {
                                Person::First => "ω",
                                Person::Second => "εις",
                                Person::Third => "ει",
                            }
                            .to_string(),
                        )],
                        Number::Dual => todo!(),
                        Number::Plural => match con.person {
                            Person::First => syllabify("ομεν").unwrap(),
                            Person::Second => syllabify("ετε").unwrap(),
                            Person::Third => syllabify("ουσι").unwrap(),
                        },
                    },
                    Tense::Imperfect => match con.number {
                        Number::Singular => vec![S::Short(
                            match con.person {
                                Person::First => "ον",
                                Person::Second => "ες",
                                Person::Third => "ε",
                            }
                            .to_string(),
                        )],
                        Number::Dual => todo!(),
                        Number::Plural => match con.person {
                            Person::First => syllabify("ομεν").unwrap(),
                            Person::Second => syllabify("ετε").unwrap(),
                            Person::Third => vec![short_s("ον")],
                        },
                    },
                },
                Voice::Middle | Voice::Passive => match con.tense {
                    Tense::Present | Tense::Future => match con.number {
                        Number::Singular => match con.person {
                            Person::First => syllabify("ομαι").unwrap(),
                            Person::Second => vec![long_s("ει")],
                            Person::Third => syllabify("εται").unwrap(),
                        },
                        Number::Dual => todo!(),
                        Number::Plural => match con.person {
                            Person::First => syllabify("ομεθα").unwrap(),
                            Person::Second => syllabify("εστε").unwrap(),
                            Person::Third => syllabify("ονται").unwrap(),
                        },
                    },
                    Tense::Imperfect => match con.number {
                        Number::Singular => match con.person {
                            Person::First => syllabify("ομην").unwrap(),
                            Person::Second => syllabify("ου").unwrap(),
                            Person::Third => syllabify("ετο").unwrap(),
                        },
                        Number::Dual => todo!(),
                        Number::Plural => match con.person {
                            Person::First => syllabify("ομεθα").unwrap(),
                            Person::Second => syllabify("εσθε").unwrap(),
                            Person::Third => syllabify("οντο").unwrap(),
                        },
                    },
                },
            },

            _ => todo!(),
        }
    }
}

impl Verb for ThematicVerb {
    fn conjugated(self, con: ConjugationProduct) -> WordSeq {
        WordSeq::from_verb(
            match con.tense {
                Tense::Present => self.present,
                Tense::Imperfect => Stem {
                    head: augmented_head(self.augment, self.present.head.clone()),
                    tail: {
                        let mut tail = vec![self.present.head.syllable()];
                        tail.extend(self.present.tail);
                        tail
                    },
                    ..self.present
                },
                Tense::Future => self.future,
            },
            ThematicVerb::ending(con),
        )
    }
    fn infinitive(self, inf: InfinitiveProduct) -> WordSeq {
        let (stem, ending) = match inf.voice {
            Voice::Active => match inf.tense {
                Tense::Present => (self.present, vec![long_s("ειν")]),
                Tense::Future => (self.future, vec![long_s("ειν")]),
                _ => todo!(),
            },
            Voice::Middle => match inf.tense {
                Tense::Present => (self.present, syllabify("εσθαι").unwrap()),
                Tense::Future => (self.future, syllabify("εσθαι").unwrap()),
                _ => todo!(),
            },
            Voice::Passive => match inf.tense {
                Tense::Present => (self.present, syllabify("εσθαι").unwrap()),
                _ => todo!(),
            },
        };
        WordSeq::from_verb(stem, ending)
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
