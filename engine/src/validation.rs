use abi_stable::std_types::RHashMap;
use frep_core::validation::ValidationErrorHandler;
use steel::steel_vm::ffi::FFIValue;

#[derive(Clone, Default, Eq, PartialEq)]
pub struct ErrorHandler {
    pub search_text_errors: Vec<String>,
    pub include_files_errors: Vec<String>,
    pub exclude_files_errors: Vec<String>,
}

impl ErrorHandler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn has_errors(&self) -> bool {
        !self.search_text_errors.is_empty()
            || !self.include_files_errors.is_empty()
            || !self.exclude_files_errors.is_empty()
    }
}

impl ValidationErrorHandler for ErrorHandler {
    fn handle_search_text_error(&mut self, _error: &str, detail: &str) {
        self.search_text_errors.push(detail.to_owned());
    }

    fn handle_include_files_error(&mut self, _error: &str, detail: &str) {
        self.include_files_errors.push(detail.to_owned());
    }

    fn handle_exclude_files_error(&mut self, _error: &str, detail: &str) {
        self.exclude_files_errors.push(detail.to_owned());
    }
}

pub(crate) fn success_response() -> FFIValue {
    let mut map = RHashMap::new();
    map.insert(FFIValue::StringV("success".into()), FFIValue::BoolV(true));
    FFIValue::HashMap(map)
}

pub(crate) fn error_response(error_type: &str, message: &str) -> FFIValue {
    let mut map = RHashMap::new();
    map.insert(FFIValue::StringV("success".into()), FFIValue::BoolV(false));
    map.insert(
        FFIValue::StringV("error-type".into()),
        FFIValue::StringV(error_type.into()),
    );
    map.insert(
        FFIValue::StringV("message".into()),
        FFIValue::StringV(message.into()),
    );
    FFIValue::HashMap(map)
}

pub(crate) fn validation_error_response(error_handler: &ErrorHandler) -> FFIValue {
    let mut map = RHashMap::new();
    map.insert(FFIValue::StringV("success".into()), FFIValue::BoolV(false));
    map.insert(
        FFIValue::StringV("error-type".into()),
        FFIValue::StringV("validation-error".into()),
    );

    // Helper to convert string vec to FFIValue array
    let add_errors = |map: &mut RHashMap<FFIValue, FFIValue>, key: &str, errors: &[String]| {
        if !errors.is_empty() {
            let ffi_errors: Vec<FFIValue> = errors
                .iter()
                .map(|e| FFIValue::StringV(e.as_str().into()))
                .collect();
            map.insert(
                FFIValue::StringV(key.into()),
                FFIValue::Vector(ffi_errors.into()),
            );
        }
    };

    add_errors(
        &mut map,
        "search-text-errors",
        &error_handler.search_text_errors,
    );
    add_errors(
        &mut map,
        "include-files-errors",
        &error_handler.include_files_errors,
    );
    add_errors(
        &mut map,
        "exclude-files-errors",
        &error_handler.exclude_files_errors,
    );

    FFIValue::HashMap(map)
}
