function parse(t) {
    return JSON.parse(t);
}

function getL(lensName, t) {
    var lens = eval(lensName)();
    return lens.get(t)
}

function setL(lensName, t, v) {
    return eval(lensName)().set(t, v)
}

function makeArray() {
    var result = [];
    for (var i = 0; i < arguments.length; i++) {
        result.push(arguments[i])
    }
    return result
}

function shallowCopy(t) {
    var result = {};
    for (var key in t) {
        result[key] = t[key];
    }
    return result;
}

function lens(field) {
    return {
        "get": function (t) {
            return t[field];
        },
        "set": function (t, v) {
            var copy = shallowCopy(t);
            copy[field] = v;
            return copy
        }
    };
}

function lensForFirstItemInList() {
    return {
        "get": function (list) {
            return list[0];
        },
        "set": function (list, item) {
            var newArray = list.slice();
            newArray[0] = item;
            return newArray
        }
    }
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

function render_form(t) {
    var name = lens_person_name_string().get(t);
    var line1 = lens_person_line1_string().get(t);
    var line2 = lens_person_line2_string().get(t);
    var html = "<form method='post' action='/person/" + name + "/edit' enctype='application/x-www-form-urlencoded'>" +
        "   <table>" +
        "      <tr><td>Name</td><td><input name ='name'type='text' value='" + name + "' readonly /></td></tr>" +
        "      <tr><td>Line1</td><td><input name='line1' type='text' value='" + line1 + "' /></td></tr>" +
        "      <tr><td>Line2</td><td><input name='line2' type='text' value='" + line2 + "' /></td></tr>" +
        "   </table>" +
        "   <input type='submit'/>" +
        "</form>";
    return html
}

function lens_root() {
    return lens("_embedded");
}




function lens_person_name_string(){ return lens("name");}; 
function lens_person_line1_string(){ return lens("line1");}; 
function lens_telephonenumber_number_string(){ return lens("number");}; 
function lens_person_telephonenumber_telephonenumber(){ return lens("telephoneNumber");}; 
function lens_person_line2_string(){ return lens("line2");}; 
