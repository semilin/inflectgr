use inflectgr::{
    ConjugationProduct, Error, InfinitiveProduct, Number, Person, Stem, Tense, ThematicVerb, Verb,
    Voice,
};

fn main() -> Result<(), Error> {
    let ago = ThematicVerb::new(Stem::new("πιστεύ".to_string())?)?;
    for tense in Tense::all() {
        println!("{tense:?}");
        for voice in [Voice::Active, Voice::Middle] {
            println!("  {voice:?}");
            for number in [Number::Singular, Number::Plural] {
                for person in Person::all() {
                    let conjugated = ago.clone().conjugated(ConjugationProduct {
                        person,
                        number,
                        tense,
                        voice,
                        ..Default::default()
                    });
                    println!("    {person:?} {number:?}: {:?}", conjugated.syls);
                }
            }
        }
    }
    println!(
        "Active Infinitive: {:?}",
        ago.clone().infinitive(InfinitiveProduct::default())
    );

    Ok(())
}
