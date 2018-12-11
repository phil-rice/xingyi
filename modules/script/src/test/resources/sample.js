function parser(t){return JSON.parse(t)}

function render_html(t) {    return "this is the rendered html"};    // The function returns the product of p1 and p2}
function render_html(t) {    return "this is the rendered json"};    // The function returns the product of p1 and p2


function lens(getter, setter) {    return {"getter": getter, "setter": setter}}
function lens_person_name (){return lens(function(t){return t.name}, function(t, v){t.name= v})}
function lens_person_address(){return lens(function(t){return t.address}, function(t, v){t.address=v})}
function lens_person_employername(){return lens(function(t){return t.employer.name}, function(t, v){ t.employer.name=v})}
function lens_person_employer_name(){return lens(function(t){return t.employer.name}, function(t, v){ t.employer.name=v})}
function lens_address_line1(){return lens(function(t){return t.line1}, function(t, v){ t.line1=v})}
function lens_address_line2(){return lens(function(t){return t.line1}, function(t, v){ t.line1=v})}

