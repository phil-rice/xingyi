function parse(t) {
    return JSON.parse(t)
}

function render_html(t) {
    return "this is the rendered html"
};
function render_json(t) {
    return JSON.stringify(t)
};


function lens_person_get(t) {
    return t.payload._embedded
}

function lens_person_set(t, v) {
    var copy =  Object.create(t);
    copy.payload._embedded = v
    return copy
}

function lens_person_name_get(t) {
    return t.name
}

function lens_person_name_set(t, v) {
    var copy = Object.create(t)
    copy.name = v
    return  copy
}

// function lens_person_employername(){return lens(function(t){return t.employer.name}, function(t, v){ t.employer.name=v})}
// function lens_person_employer_name(){return lens(function(t){return t.employer.name}, function(t, v){ t.employer.name=v})}
// function lens_address_line1(){return lens(function(t){return t.line1}, function(t, v){ t.line1=v})}
// function lens_address_line2(){return lens(function(t){return t.line1}, function(t, v){ t.line1=v})}

