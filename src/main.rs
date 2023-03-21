use nom::character::complete::alpha1;
use nom::combinator::map_res;
use nom::combinator::recognize;
use nom::Err;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{digit1, space0},
    combinator::map,
    multi::separated_list1,
    sequence::{delimited, tuple},
    IResult,
};

#[derive(Debug, PartialEq, Clone)]
pub enum AST {
    Equals { field: String, value: Value },
    NotEquals { field: String, value: Value },
    Contains { field: String, value: Value },
    And(Box<AST>, Box<AST>),
    Or(Box<AST>, Box<AST>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    String(String),
    Int(i32),
    VecString(Vec<String>),
    VecInt(Vec<i32>),
}

pub struct Filter {
    ast: AST,
}

impl Filter {
    pub fn new(ast: AST) -> Self {
        Self { ast }
    }

    pub fn apply<T: Filterable + Clone>(&self, items: &[T]) -> Vec<T> {
        items
            .iter()
            .filter(|item| item.matches_ast(&self.ast))
            .cloned()
            .collect()
    }
}

pub trait Filterable: Sized {
    fn get_property_value(&self, property_name: &str) -> Option<Value>;
    fn matches_ast(&self, ast: &AST) -> bool;
}

fn parse_field(input: &str) -> IResult<&str, String> {
    map_res(recognize(tuple((alpha1, space0))), |s: &str| {
        Ok::<String, Err<(&str, nom::error::ErrorKind)>>(s.trim().to_string())
    })(input)
}

fn parse_value(input: &str) -> IResult<&str, Value> {
    let (input, value) = alt((
        map(digit1, |s: &str| Value::Int(s.parse::<i32>().unwrap())),
        map(
            delimited(tag("\""), take_while1(|c: char| c != '"'), tag("\"")),
            |s: &str| Value::String(s.to_string()),
        ),
    ))(input)?;
    Ok((input, value))
}

fn parse_vec_string(input: &str) -> IResult<&str, Vec<String>> {
    let (input, values) =
        delimited(tag("["), separated_list1(tag(","), parse_value), tag("]"))(input)?;
    let values = values
        .into_iter()
        .filter_map(|v| match v {
            Value::String(s) => Some(s),
            _ => None,
        })
        .collect();
    Ok((input, values))
}

fn parse_vec_int(input: &str) -> IResult<&str, Vec<i32>> {
    let (input, values) =
        delimited(tag("["), separated_list1(tag(","), parse_value), tag("]"))(input)?;
    let values = values
        .into_iter()
        .filter_map(|v| match v {
            Value::Int(i) => Some(i),
            _ => None,
        })
        .collect();
    Ok((input, values))
}

fn parse_ast(input: &str) -> IResult<&str, AST> {
    let (input, ast) = alt((
        map(
            tuple((tag("=="), space0, parse_field, space0, parse_value)),
            |(_, _, field, _, value)| AST::Equals { field, value },
        ),
        map(
            tuple((tag("!="), space0, parse_field, space0, parse_value)),
            |(_, _, field, _, value)| AST::NotEquals { field, value },
        ),
        map(
            tuple((
                tag("in"),
                space0,
                parse_field,
                space0,
                alt((
                    map(parse_vec_string, Value::VecString),
                    map(parse_vec_int, Value::VecInt),
                    map(parse_value, |x| x),
                )),
            )),
            |(_, _, field, _, value)| AST::Contains { field, value },
        ),
        map(
            tuple((
                tag("&&"),
                space0,
                delimited(tag("("), parse_ast, tag(")")),
                space0,
                delimited(tag("("), parse_ast, tag(")")),
            )),
            |(_, _, left, _, right)| AST::And(Box::new(left), Box::new(right)),
        ),
        map(
            tuple((
                tag("||"),
                space0,
                delimited(tag("("), parse_ast, tag(")")),
                space0,
                delimited(tag("("), parse_ast, tag(")")),
            )),
            |(_, _, left, _, right)| AST::Or(Box::new(left), Box::new(right)),
        ),
    ))(input)?;
    Ok((input, ast))
}

fn parse_query(input: &str) -> IResult<&str, AST> {
    delimited(space0, parse_ast, space0)(input)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Person {
    name: String,
    age: u32,
    interests: Vec<String>,
}

impl Person {
    fn internal_matches_ast(&self, ast: &AST) -> bool {
        match ast {
            AST::Equals { field, value } => self.get_property_value(field) == Some(value.clone()),
            AST::NotEquals { field, value } => {
                self.get_property_value(field) != Some(value.clone())
            }
            _ => false,
        }
    }
    fn matches_contains(&self, field: &str, value: &Value) -> bool {
        match (self.get_property_value(field), value) {
            (Some(Value::String(ref s)), Value::String(ref sub)) => s.contains(sub),
            (Some(Value::VecString(ref v)), Value::String(ref sub)) => {
                v.iter().any(|s| s.contains(sub))
            }
            (Some(Value::String(ref s)), Value::VecString(ref v)) => {
                v.iter().any(|sub| s.contains(sub))
            }
            (Some(Value::VecString(ref v1)), Value::VecString(ref v2)) => {
                v1.iter().any(|s| v2.iter().any(|sub| s.contains(sub)))
            }
            _ => false,
        }
    }

    fn matches_and_or(&self, ast: &AST) -> bool {
        match ast {
            AST::And(left, right) => {
                self.matches_ast(left.as_ref()) && self.matches_ast(right.as_ref())
            }
            AST::Or(left, right) => {
                self.matches_ast(left.as_ref()) || self.matches_ast(right.as_ref())
            }
            _ => false,
        }
    }
}

impl Filterable for Person {
    fn get_property_value(&self, property_name: &str) -> Option<Value> {
        match property_name {
            "name" => Some(Value::String(self.name.clone())),
            "age" => Some(Value::Int(self.age as i32)),
            "interests" => Some(Value::VecString(self.interests.clone())),
            _ => None,
        }
    }
    fn matches_ast(&self, ast: &AST) -> bool {
        match ast {
            AST::Equals { field: _, value: _ } | AST::NotEquals { field: _, value: _ } => {
                self.internal_matches_ast(ast)
            }
            AST::Contains { field, value } => self.matches_contains(field, value),
            AST::And(_, _) | AST::Or(_, _) => self.matches_and_or(
                ast,
            ),
        }
    }
}

fn main() {
    let input = "== name \"Alice\"";
    let result = parse_query(input);

    let people = vec![
        Person {
            name: "Alice".to_string(),
            age: 30,
            interests: vec!["reading".to_string(), "hiking".to_string()],
        },
        Person {
            name: "Bob".to_string(),
            age: 25,
            interests: vec!["swimming".to_string(), "cooking".to_string()],
        },
        Person {
            name: "Charlie".to_string(),
            age: 35,
            interests: vec!["hiking".to_string(), "painting".to_string()],
        },
    ];

    match result {
        Ok((_, ast)) => {
            let filtered_people: Vec<Person> = people
                .into_iter()
                .filter(|person| person.matches_ast(&ast))
                .collect();
            println!("Filtered people: {:?}", filtered_people);
        }
        Err(e) => println!("Error: {:?}", e),
    }
}

impl AST {
    pub fn apply(&self, people: &[Person]) -> Vec<Person> {
        people
            .iter()
            .filter(|person| person.matches_ast(self))
            .cloned()
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_filtering() {
        let alice = Person {
            name: "Alice".to_string(),
            age: 30,
            interests: vec!["reading".to_string(), "hiking".to_string()],
        };
        let bob = Person {
            name: "Bob".to_string(),
            age: 20,
            interests: vec!["swimming".to_string(), "cooking".to_string()],
        };
        let carol = Person {
            name: "Carol".to_string(),
            age: 25,
            interests: vec!["hiking".to_string(), "painting".to_string()],
        };

        let people = vec![alice.clone(), bob.clone(), carol.clone()];

        let test_cases = vec![
            ("== name \"Alice\"", vec![0]),
            ("!= name \"Alice\"", vec![1, 2]),
            ("== age 30", vec![0]),
            ("!= age 30", vec![1, 2]),
            ("in interests [\"reading\"]", vec![0]),
            ("in interests [\"cooking\"]", vec![1]),
            ("in interests [\"hiking\"]", vec![0, 2]),
            ("&& (== name \"Alice\") (== age 30)", vec![0]),
            ("|| (== name \"Alice\") (== name \"Bob\")", vec![0, 1]),
            ("in name [\"Alice\",\"Bob\"]", vec![0, 1]),
            ("in interests \"hiking\"", vec![0, 2]),
            (
                "|| (== name \"Alice\") (== name \"Bob\") (== name \"Eve\")",
                vec![0, 1],
            ),
            ("&& (in interests [\"hiking\"]) (== age 25)", vec![2]),
            (
                "|| (&& (== name \"Alice\") (in interests [\"hiking\"])) (== age 20)",
                vec![0, 1],
            ),
            (
                "&& (in interests [\"hiking\"]) (|| (== age 20) (== age 25))",
                vec![2],
            ),
        ];

        for (input, expected_indices) in test_cases {
            println!("Testing input: {}", input); // Add this line to print the input being tested
            let ast = parse_ast(input).unwrap_or_else(|err| {
                panic!("Failed to parse input '{}': {:?}", input, err);
            });
            println!("Testing AST: {:?}", &ast.1);

            let filtered_people: Vec<Person> = ast.1.apply(&people);
            let expected_people: Vec<Person> = expected_indices
                .iter()
                .map(|index| people[*index].clone())
                .collect();

            assert_eq!(filtered_people, expected_people);
        }
    }
}
