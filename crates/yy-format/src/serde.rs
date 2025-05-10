use std::{fmt, mem};

use serde::de;
use thiserror::Error;

use crate::value::{Array, Object, Value};

#[derive(Debug, Error)]
pub enum Error {
    #[error("{0}")]
    Message(String),
    #[error("expected {expected}, found {found}")]
    TypeError {
        expected: &'static str,
        found: &'static str,
    },
}

impl de::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error::Message(msg.to_string())
    }
}

pub fn from_value<'a, T: de::Deserialize<'a>>(value: Value) -> Result<T, Error> {
    T::deserialize(Deserializer::from_value(value))
}

pub struct Deserializer {
    value: Value,
}

impl Deserializer {
    pub fn from_value(value: Value) -> Self {
        Self { value }
    }
}

fn type_name(value: &Value) -> &'static str {
    match value {
        Value::Null => "null",
        Value::Boolean(_) => "bool",
        Value::Integer(_) => "integer",
        Value::Float(_) => "float",
        Value::String(_) => "string",
        Value::Object(_) => "object",
        Value::Array(_) => "array",
    }
}

impl<'a> de::Deserializer<'a> for Deserializer {
    type Error = Error;

    fn deserialize_any<V: de::Visitor<'a>>(self, visitor: V) -> Result<V::Value, Error> {
        match self.value {
            Value::Null => self.deserialize_unit(visitor),
            Value::Boolean(_) => self.deserialize_bool(visitor),
            Value::Integer(_) => self.deserialize_i64(visitor),
            Value::Float(_) => self.deserialize_f64(visitor),
            Value::String(s) => visitor.visit_string(s),
            Value::Object(_) => self.deserialize_map(visitor),
            Value::Array(_) => self.deserialize_seq(visitor),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        match self.value {
            Value::Boolean(b) => visitor.visit_bool(b),
            v => Err(Error::TypeError {
                expected: "bool",
                found: type_name(&v),
            }),
        }
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        match self.value {
            Value::Integer(i) => visitor.visit_i64(i),
            v => Err(Error::TypeError {
                expected: "integer",
                found: type_name(&v),
            }),
        }
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        self.deserialize_f64(visitor)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        match self.value {
            Value::Integer(i) => visitor.visit_f64(i as f64),
            Value::Float(f) => visitor.visit_f64(f),
            v => Err(Error::TypeError {
                expected: "number",
                found: type_name(&v),
            }),
        }
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        match self.value {
            Value::String(s) => visitor.visit_string(s),
            v => Err(Error::TypeError {
                expected: "string",
                found: type_name(&v),
            }),
        }
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        Err(de::Error::custom("no support for deserialize_bytes"))
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        self.deserialize_bytes(visitor)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        match self.value {
            Value::Null => visitor.visit_none(),
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        match self.value {
            Value::Null => visitor.visit_unit(),
            v => Err(Error::TypeError {
                expected: "null",
                found: type_name(&v),
            }),
        }
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        if let Value::Array(array) = self.value {
            visitor.visit_seq(Seq::new(array))
        } else {
            Err(Error::TypeError {
                expected: "array",
                found: type_name(&self.value),
            })
        }
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        if let Value::Array(array) = self.value {
            visitor.visit_seq(Seq::new(array))
        } else {
            Err(Error::TypeError {
                expected: "array",
                found: type_name(&self.value),
            })
        }
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        self.deserialize_tuple(len, visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        if let Value::Object(obj) = self.value {
            visitor.visit_map(Map::new(obj))
        } else {
            Err(Error::TypeError {
                expected: "object",
                found: type_name(&self.value),
            })
        }
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        match self.value {
            Value::Object(object) => {
                if object.len() == 1 {
                    let (key, value) = object.into_iter().next().unwrap();
                    visitor.visit_enum(Enum::new(key, value))
                } else {
                    Err(de::Error::custom(
                        "enum object does not have exactly one entry",
                    ))
                }
            }
            v => visitor.visit_enum(UnitEnum::new(v)),
        }
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        self.deserialize_any(visitor)
    }
}

pub struct Seq {
    array: Array,
    ind: usize,
}

impl Seq {
    fn new(array: Array) -> Self {
        Self { array, ind: 0 }
    }
}

impl<'a> de::SeqAccess<'a> for Seq {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Error>
    where
        T: de::DeserializeSeed<'a>,
    {
        if let Some(v) = self.array.get_mut(self.ind) {
            let res =
                Some(seed.deserialize(Deserializer::from_value(mem::replace(v, Value::Null)))?);
            self.ind += 1;
            Ok(res)
        } else {
            Ok(None)
        }
    }
}

pub struct Map {
    pairs: Vec<(String, Value)>,
    next_value: Option<Value>,
}

impl Map {
    fn new(object: Object) -> Self {
        Self {
            pairs: object.into_iter().collect(),
            next_value: None,
        }
    }
}

impl<'a> de::MapAccess<'a> for Map {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Error>
    where
        K: de::DeserializeSeed<'a>,
    {
        if let Some((key, value)) = self.pairs.pop() {
            self.next_value = Some(value);
            seed.deserialize(Deserializer::from_value(Value::String(key)))
                .map(Some)
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Error>
    where
        V: de::DeserializeSeed<'a>,
    {
        seed.deserialize(Deserializer::from_value(
            self.next_value
                .take()
                .expect("must call `next_key_seed` first"),
        ))
    }
}

pub struct Enum {
    key: String,
    value: Value,
}

impl Enum {
    fn new(key: String, value: Value) -> Self {
        Self { key, value }
    }
}

impl<'a> de::EnumAccess<'a> for Enum {
    type Error = Error;
    type Variant = Variant;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Variant), Error>
    where
        V: de::DeserializeSeed<'a>,
    {
        Ok((
            seed.deserialize(Deserializer::from_value(Value::String(self.key)))?,
            Variant::new(self.value),
        ))
    }
}

pub struct Variant {
    value: Value,
}

impl Variant {
    fn new(value: Value) -> Self {
        Self { value }
    }
}

impl<'a> de::VariantAccess<'a> for Variant {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Error> {
        de::Deserialize::deserialize(Deserializer::from_value(self.value))
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Error>
    where
        T: de::DeserializeSeed<'a>,
    {
        seed.deserialize(Deserializer::from_value(self.value))
    }

    fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        de::Deserializer::deserialize_tuple(Deserializer::from_value(self.value), len, visitor)
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'a>,
    {
        de::Deserializer::deserialize_map(Deserializer::from_value(self.value), visitor)
    }
}

pub struct UnitEnum {
    key: Value,
}

impl UnitEnum {
    fn new(key: Value) -> Self {
        Self { key }
    }
}

impl<'a> de::EnumAccess<'a> for UnitEnum {
    type Error = Error;
    type Variant = UnitVariant;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, UnitVariant), Error>
    where
        V: de::DeserializeSeed<'a>,
    {
        Ok((
            seed.deserialize(Deserializer::from_value(self.key))?,
            UnitVariant::new(),
        ))
    }
}

pub struct UnitVariant {}

impl UnitVariant {
    fn new() -> Self {
        Self {}
    }
}

impl<'de> de::VariantAccess<'de> for UnitVariant {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, _seed: T) -> Result<T::Value, Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        Err(Error::TypeError {
            expected: "table",
            found: "non-table",
        })
    }

    fn tuple_variant<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        Err(Error::TypeError {
            expected: "table",
            found: "non-table",
        })
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        Err(Error::TypeError {
            expected: "table",
            found: "non-table",
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_yy_deserialize() {
        #[derive(Debug, PartialEq, serde::Deserialize)]
        struct Test {
            str_field: String,
            num_field: f64,
            bool_field: bool,
            arr_field: Vec<i64>,
        }

        let test: Test = from_value(
            Value::parse(
                r#"
                    {
                        "str_field": "foo",
                        "num_field": 1,
                        "bool_field": true,
                        "arr_field": [1, 2, 3],
                    }
                
                "#,
            )
            .unwrap(),
        )
        .unwrap();

        assert_eq!(
            test,
            Test {
                str_field: "foo".to_owned(),
                num_field: 1.0,
                bool_field: true,
                arr_field: vec![1, 2, 3],
            }
        )
    }
}
