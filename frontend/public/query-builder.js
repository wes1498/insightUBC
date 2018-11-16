/**
 * Builds a query object using the current document object model (DOM).
 * Must use the browser's global document object {@link https://developer.mozilla.org/en-US/docs/Web/API/Document}
 * to read DOM information.
 *
 * @returns query object adhering to the query EBNF
 */

CampusExplorer.buildQuery = function() {
    let query = {};

    let id = document.getElementsByTagName("nav")[0];
    id = id.getElementsByClassName("nav-item tab active")[0];
    id = id.getAttribute("data-type");
    let where = {};

    if (id === "courses") {
        let courseTab = document.getElementById("tab-courses");

        let conditions = courseTab.getElementsByClassName("form-group conditions")[0];

        let condType = conditions.getElementsByClassName("control-group condition-type")[0].getElementsByTagName("input");

        let cond;
        if (condType[0].getElementById("courses-conditiontype-all") && condType[0].getAttribute("checked") !== null) {
            cond = "AND";
        } else if (condType[1].getElementById("courses-conditiontype-any") && condType[1].getAttribute("checked") !== null) {
            cond = "OR";
        } else if (condType[2].getElementById("courses-conditiontype-none") && condType[2].getAttribute("checked") !== null) {
            cond = "NOT";
        }
        where = whereOBJHelper(conditions, cond);

    } else if (id === "rooms") {
        let roomsTab = document.getElementById("tab-rooms");

        let conditions = roomsTab.getElementsByClassName("form-group conditions")[0];

        let condType = conditions.getElementsByClassName("control-group condition-type")[0].getElementsByTagName("input");

        let cond;
        if (condType[0].getElementById("rooms-conditiontype-all") && condType[0].getAttribute("checked") !== null) {
            cond = "AND";
        } else if (condType[1].getElementById("rooms-conditiontype-any") && condTypecondType[1].getAttribute("checked") !== null) {
            cond = "OR";
        } else if (condType[2].getElementById("rooms-conditiontype-none") && condType[2].getAttribute("checked") !== null) {
            cond = "NOT";
        }
        where = whereOBJHelper(conditions, cond);
    }
    // bug
    let tab;
    if (courseTab) {
        tab = courseTab;
    } else {
        tab = roomsTab;
    }

    query["WHERE"] = where; // WHERE REQUIRED

    options["COLUMNS"] = columnsOBJHelper(tab); // COLUMNS REQUIRED

    if (orderOBJHelper(tab).keys.length > 0) {

        options["ORDER"] = orderOBJHelper(tab); // ORDER NOT REQUIRED
    }

    query["OPTIONS"] = options; // OPTIONS REQUIRED

    let transformations = {};

    if (groupOBJHelper(tab).length > 0) {
        query["TRANSFORMATIONS"] = groupOBJHelper(tab); // TRANSFORMATIONS NOT REQRUIRED
        query["APPLY"] = applyOBJHelper(tab); // APPLY REQUIRED IF TRANSFORMATIONS EXISTS
    }

    return query;
};


function whereOBJHelper(conditions, cond) {
    let where = {};

    if (conditions.getElementsByClassName("conditions-container").length !== 0) {

        let filters = [];

        conditions.getElementsByClassName("conditions-container")[0]
            .getElementsByClassName("control-group condition")
            .forEach(function (eachControl) {

                let not = eachControl.firstChild.getElementsByTagName("input")[0];
                not = not.getAttribute("checked"); // grab checked box


                let compareKey;
                let keys = eachControl.getElementsByClassName("control fields")[0];
                keys = keys.getElementsByTagName("option"); // grab value = <<KEY>>

                for (let key of keys) {
                    if (key.getAttribute("selected") !== null) {
                        compareKey = key.getAttribute("value"); // set compareKey to the value of key
                        break;
                    }
                }

                let operation;
                let operators = eachControl.getElementsByClassName("control operators")[0];
                operators = operators.getElementsByTagName("option");
                for (let operator of operators) {
                    if (operator.getAttribute("selected") !== null) {
                        operation = operator.getAttribute("value");
                        // operation = GT | LT | EQ | IS
                        break
                    }
                }
                let term;
                // let control = eachControl.getElementsByClassName("control term")[0]; // first Element Child;
                if (operation !== "IS") { // GT LT EQ
                    term = Number(eachControl.getElementsByClassName("control term")[0].getAttribute("value")); // turn into a number
                } else if (operation === "IS") {
                    term = eachControl.getElementsByClassName("control term")[0].getAttribute("value"); // leave comparator as string
                }

                // grab key (id_key = term)
                let refine = {};
                refine[id + "_" + compareKey] = term;
                // refine [ id_shortname ] = value
                let filter = {}; // filter object
                filter[operation] = refine;
                // "EQ": {
                //      courses_avg = 70;
                //      }
                if (not !== null) { // if NOT box was checked
                    filters.push({"NOT": filter});
                    // "NOT": {
                    //          "EQ": {} (FILTER)
                    //        }
                } else if (not === null) {
                    filters.push(filter);
                    // filter will be populated regularly
                    // "WHERE": {
                    //      "EQ": {
                    //      courses_avg = 70;
                    //        }
                    //     }
                }

            });
        const OR = {};

        if (filters.length !== 1) {
            if (cond !== "NOT") {
                where[cond] = filters;
            } else if (cond === "NOT") {
                OR["OR"] = filters;
                where["NOT"] = OR;
            }
        } else if (filters.length === 1) {
            if (cond !== "NOT") {
                where = filters;
            } else if (cond === "NOT") {
                where["NOT"] = filters;
            }
        }

        return where;
    }
}

function columnsOBJHelper(tab) {
    let listCols = [];

    // for each COLUMN OPTION
    tab.getElementsByClassName("form-group columns")[0].columns.getElementsByClassName("control field").forEach(function (val) {
        // for each val or option in form-groups
        if (val.getAttribute("checked") !== null) {
            let key = id + "_" + option.firstElementChild.getAttribute("value");
            listCols.push(key);
        }
    });
    // for each TRANSFORMATION GROUP KEY
    tab.getElementsByClassName("form-group columns")[0].getElementsByClassName("control transformation").forEach(function (trans) {
        if (trans.getAttribute("checked") !== null) {
            let key = trans.firstElementChild.getAttribute("value");
            listCols.push(key);
        }
    });
    return listCols;
}
function orderOBJHelper(tab) {
    let order = {};


    let keys = [];
    tab.getElementsByClassName("form-group order")[0]
        .getElementsByTagName("select")[0]
        .getElementsByTagName("option")
        .forEach(function (option) {
            let key;
            if (option.getAttribute("selected") !== null) {
                key = id + "_" + option.getAttribute("value");
            } else if (!option.getAttribute("selected")) {
                key = option.getAttribute("value");
            }
            keys.push(key);
        });
    if (keys.length === 0) {
        return order;
    }
    order["keys"] = keys;
    let dir;
    if (tab.getElementsByClassName("form-group order")[0]
        .getElementsByClassName("control descending")[0]
        .getElementsByTagName("input")[0]
        .getAttribute("checked") !== null) {
        dir = "DOWN";
    } else {
        dir = "UP";
    }

    order["dir"] = dir;

    return order;
}
function groupOBJHelper(tab) {

    const keys = [];

    tab.getElementsByClassName("form-group groups")[0].getElementsByTagName("input").forEach(function (val) {
        if (val.getAttribute("checked") !== null ){
            let key = id + "_" + val.getAttribute("value");
            keys.push(key);
        }
    });

    return keys;
}

function applyOBJHelper(tab) {
    // const transformations = tab.getElementsByClassName("form-group transformations")[0];
    const applys = [];

    tab.getElementsByClassName("form-group transformations")[0]
        .getElementsByClassName("control-group transformation")
        .forEach(function (transformation) {
            // "maxSeats"
            let madeKey = transformation.getElementsByClassName("control term")[0].getElementsByTagName("input")[0].getAttribute("value");

            let operator;
            // operator = COUNT | MAX | AVG | MIN | SUM
            transformation.getElementsByClassName("control operators")[0]
                .getElementsByTagName("option").find(function (operation) {
                if (operation.getAttribute("selected") !== null) {
                    operator = operation.getAttribute("value");
                }
            });

            // transformation.getElementsByClassName("control operators")[0]
            //     .getElementsByTagName("option")
            //     .forEach(function (operation) {
            //     if (operation.getAttribute("selected") !== null) {
            //         operator = operation.getAttribute("value");
            //     }
            // });

            let key;

            // key = oneOf audit-avg-dept...
            transformation.getElementsByClassName("control fields")[0]
                .getElementsByTagName("option").find(function (field) {
                if (field.getAttribute("selected") !== null) {
                    key = field.getAttribute("value");

                }
            });

            // transformation.getElementsByClassName("control fields")[0]
            //     .getElementsByTagName("option")
            //     .forEach(function (field) {
            //         if (field.getAttribute("selected") !== null) {
            //             key = field.getAttribute("value");
            //
            //         }
            //     });

            let controlOperators = {};
            controlOperators[transformation.getElementsByClassName("control operators")[0]] = key;
            // controlOperators["COUNT": KEY, "AVG: KEY ...]

            const apply = {};

            // apply{ "COUNT": "rooms_seats" }
            apply[operator] = key;
            let applyObj = {};

            // applyObj ["maxSeats"
            applyObj[madeKey] = apply;
            applys.push(applyObj);


            // applys [{
            //          "maxSeats": {
            //                  "COUNT": "rooms_seats";
            //              }
            //              ...
            //          }]
        });

    return applys;
}
