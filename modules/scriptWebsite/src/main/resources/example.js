/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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

function makeArray() {
    var result = []
    for (var i = 0; i < arguments.length; i++) {
        result.push(arguments[i])
    }
    return result
}

function shallowCopy(t) {
    var result = {}
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
            var copy = shallowCopy(t)
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
    return  lens("_embedded");
}



function lens_lens_address_line2_string(){ return lens("line2");}; 
function lens_lens_person_name_string(){ return lens("name");}; 
function lens_lens_person_telephonenumber_telephonenumber(){ return lens("telephoneNumber");}; 
function lens_lens_address_line1_string(){ return lens("line1");}; 
function lens_lens_person_addresses_addresslist(){ return lens("addresses");}; 
function lens_lens_telephonenumber_number_string(){ return lens("number");}; 
legacy_person_line1_lens
legacy_person_line1_lens
legacy_address
