function parse(t) {
    return JSON.parse(t)
}
function getL(lensName, t) {
    var lens = eval(lensName)()
    return lens.get(t)
}

function setL(lensName, t, v) {
    return eval(lensName)().set(t, v)
}

function lens(field) {
    return {
        "get": function (t) {
            return t[field];
        },
        "set": function (t,v) {
            var copy = JSON.parse(JSON.stringify(t));
            copy[field] = v
            return copy
        }
    };
}

function compose(l1, l2) {
    return {
        "get": function (t) {
            return l2.get(l1.get(t));
        },
        "set": function (t, v) {
            return l1.set(t, l2.set(l1.get(t), v));
        }
    }
}


function render_json(t) {
    return JSON.stringify(t)
};

function render_pretty(t) {
    return JSON.stringify(t, null, 2)
};

function lens_root() {
    return compose(lens("payload"), lens("_embedded"));
}



function lens_person_address(){ return lens("address");}; 
function lens_person_name(){ return lens("name");}; 
function lens_address_line1(){ return lens("line1");}; 
function lens_person_telephone(){ return lens("telephoneNumber");}; 
function lens_address_line2(){ return lens("line2");}; 
