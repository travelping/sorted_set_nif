extern crate rustler;

mod bucket;
mod configuration;
mod sorted_set;
mod supported_term;

use std::sync::Mutex;

use jemallocator::Jemalloc;
use rustler::ResourceArc;
use rustler::types::tuple::get_tuple;
use rustler::{Atom, Env, Term, Error, NifResult};

use crate::configuration::Configuration;
use crate::sorted_set::SortedSet;
use crate::supported_term::SupportedTerm;

#[global_allocator]
static GLOBAL_ALLOCATOR: Jemalloc = Jemalloc;

mod atoms {
    rustler::atoms! {
        // Common Atoms
        ok,
        error,

        // Resource Atoms
        bad_reference,
        lock_fail,

        // Success Atoms
        added,
        duplicate,
        removed,

        // Error Atoms
        unsupported_type,
        not_found,
        index_out_of_bounds,
        max_bucket_size_exceeded,
    }
}

pub struct SortedSetResource(Mutex<SortedSet>);

type SortedSetArc = ResourceArc<SortedSetResource>;

#[derive(Debug, PartialEq)]
pub enum AddResult {
    Added(usize),
    Duplicate(usize),
}

#[derive(Debug, PartialEq)]
pub enum RemoveResult {
    Removed(usize),
    NotFound,
}

#[derive(Debug, PartialEq)]
pub enum FindResult {
    Found {
        bucket_idx: usize,
        inner_idx: usize,
        idx: usize,
    },
    NotFound,
}

#[derive(Debug, PartialEq)]
pub enum AppendBucketResult {
    Ok,
    MaxBucketSizeExceeded,
}

rustler::init!(
    "Elixir.Discord.SortedSet.NifBridge",
    load = load
);

#[rustler::resource_impl()]
impl rustler::Resource for SortedSetResource {}

fn load(_env: Env, _info: Term) -> bool {
    true
}

#[rustler::nif]
fn empty(initial_item_capacity: usize, max_bucket_size: usize) -> (Atom, SortedSetArc) {
    let initial_set_capacity: usize = (initial_item_capacity / max_bucket_size) + 1;

    let configuration = Configuration {
        max_bucket_size,
        initial_set_capacity,
    };

    let resource = ResourceArc::new(SortedSetResource(Mutex::new(SortedSet::empty(
        configuration,
    ))));

    (atoms::ok(), resource)
}

#[rustler::nif]
fn new(initial_item_capacity: usize, max_bucket_size: usize) -> (Atom, SortedSetArc) {
    let initial_set_capacity: usize = (initial_item_capacity / max_bucket_size) + 1;

    let configuration = Configuration {
        max_bucket_size,
        initial_set_capacity,
    };

    let resource = ResourceArc::new(SortedSetResource(Mutex::new(SortedSet::new(configuration))));

    (atoms::ok(), resource)
}

#[rustler::nif]
fn append_bucket(resource: ResourceArc<SortedSetResource>, term: Term) -> Result<Atom, Atom> {
    let items = match convert_to_supported_term(&term) {
        Some(SupportedTerm::List(terms)) => terms,
        _ => return Err(atoms::unsupported_type()),
    };

    let mut set = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    match set.append_bucket(items) {
        AppendBucketResult::Ok => Ok(atoms::ok()),
        AppendBucketResult::MaxBucketSizeExceeded => Err(atoms::max_bucket_size_exceeded()),
    }
}

#[rustler::nif]
fn add(resource: ResourceArc<SortedSetResource>, term: Term) -> Result<(Atom, usize), Atom> {
    let item = match convert_to_supported_term(&term) {
        None => return Err(atoms::unsupported_type()),
        Some(term) => term,
    };

    let mut set = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    match set.add(item) {
        AddResult::Added(idx) => Ok((atoms::added(), idx)),
        AddResult::Duplicate(idx) => Ok((atoms::duplicate(), idx)),
    }
}

#[rustler::nif]
fn remove(resource: ResourceArc<SortedSetResource>, term: Term) -> Result<(Atom, usize), Atom> {
    let item = match convert_to_supported_term(&term) {
        None => return Err(atoms::unsupported_type()),
        Some(term) => term,
    };

    let mut set = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    match set.remove(&item) {
        RemoveResult::Removed(idx) => Ok((atoms::removed(), idx)),
        RemoveResult::NotFound => Err(atoms::not_found()),
    }
}

#[rustler::nif]
fn size(resource: ResourceArc<SortedSetResource>) -> Result<usize, Atom> {
    let set = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    Ok(set.size())
}

#[rustler::nif]
fn to_list(resource: ResourceArc<SortedSetResource>) -> Result<Vec<SupportedTerm>, Atom> {
    let set = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    Ok(set.to_vec())
}

#[rustler::nif]
fn at(resource: ResourceArc<SortedSetResource>, index: usize) -> Result<SupportedTerm, Atom> {
    let set = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    match set.at(index) {
        None => Err(atoms::index_out_of_bounds()),
        Some(value) => Ok(value.clone()),
    }
}

#[rustler::nif]
fn slice(
    resource: ResourceArc<SortedSetResource>,
    start: usize,
    amount: usize,
) -> Result<Vec<SupportedTerm>, Atom> {
    let set = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    Ok(set.slice(start, amount))
}

#[rustler::nif]
fn find_index(resource: ResourceArc<SortedSetResource>, term: Term) -> Result<usize, Atom> {
    let item = match convert_to_supported_term(&term) {
        None => return Err(atoms::unsupported_type()),
        Some(term) => term,
    };

    let set = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    match set.find_index(&item) {
        FindResult::Found {
            bucket_idx: _,
            inner_idx: _,
            idx,
        } => Ok(idx),
        FindResult::NotFound => Err(atoms::not_found()),
    }
}

#[rustler::nif]
fn debug(resource: ResourceArc<SortedSetResource>) -> Result<String, Atom> {
    let set = match resource.0.try_lock() {
        Err(_) => return Err(atoms::lock_fail()),
        Ok(guard) => guard,
    };

    Ok(set.debug())
}

fn convert_to_supported_term(term: &Term) -> Option<SupportedTerm> {
    if term.is_number() {
        match term.decode() {
            Ok(i) => Some(SupportedTerm::Integer(i)),
            Err(_) => None,
        }
    } else if term.is_atom() {
        match term.atom_to_string() {
            Ok(a) => Some(SupportedTerm::Atom(a)),
            Err(_) => None,
        }
    } else if term.is_tuple() {
        match get_tuple(*term) {
            Ok(t) => {
                let initial_length = t.len();
                let inner_terms: Vec<SupportedTerm> = t
                    .into_iter()
                    .filter_map(|i: Term| convert_to_supported_term(&i))
                    .collect();
                if initial_length == inner_terms.len() {
                    Some(SupportedTerm::Tuple(inner_terms))
                } else {
                    None
                }
            }
            Err(_) => None,
        }
    } else if term.is_list() {
        match term.decode::<Vec<Term>>() {
            Ok(l) => {
                let initial_length = l.len();
                let inner_terms: Vec<SupportedTerm> = l
                    .into_iter()
                    .filter_map(|i: Term| convert_to_supported_term(&i))
                    .collect();
                if initial_length == inner_terms.len() {
                    Some(SupportedTerm::List(inner_terms))
                } else {
                    None
                }
            }
            Err(_) => None,
        }
    } else if term.is_binary() {
        match term.decode() {
            Ok(b) => Some(SupportedTerm::Bitstring(b)),
            Err(_) => None,
        }
    } else {
        None
    }
}


#[derive(rustler::NifMap)]
pub struct JemallocStats {
    active: u64,
    allocated: u64,
    epoch: u64,
    mapped: u64,
    metadata: u64,
    resident: u64,
    retained: u64,
}

macro_rules! jemalloc_value {
    ($name: ident) => {
        jemalloc_ctl::$name::mib()
            .and_then(|x| x.read())
            .map(|x| x as u64)
            .map_err(|x| Error::Term(Box::new(x.to_string())))
    };
}

macro_rules! jemalloc_stat_value {
    ($name: ident) => {
        jemalloc_ctl::stats::$name::mib()
            .and_then(|x| x.read())
            .map(|x| x as u64)
            .map_err(|x| Error::Term(Box::new(x.to_string())))
    };
}

#[rustler::nif]
pub fn jemalloc_allocation_info() -> NifResult<JemallocStats> {
    jemalloc_ctl::epoch::mib().and_then(|x| x.advance()).ok();

    Ok(JemallocStats {
        active: jemalloc_stat_value!(active)?,
        allocated: jemalloc_stat_value!(allocated)?,
        epoch: jemalloc_value!(epoch)?,
        mapped: jemalloc_stat_value!(mapped)?,
        metadata: jemalloc_stat_value!(metadata)?,
        resident: jemalloc_stat_value!(resident)?,
        retained: jemalloc_stat_value!(retained)?,
    })
}
