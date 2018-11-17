/**
 * Builds a query object using the current document object model (DOM).
 * Must use the browser's global document object {@link https://developer.mozilla.org/en-US/docs/Web/API/Document}
 * to read DOM information.
 *
 * @returns query object adhering to the query EBNF
 */
CampusExplorer.buildQuery = function() {
    let query = {
        "WHERE": {},
        "OPTIONS": {
            "COLUMNS": []

        }
    };

    let id = document.getElementsByTagName("nav")[0];
    id = id.getElementsByClassName("nav-item tab active")[0];
    id = id.getAttribute("data-type");
    let where = {};

    let courseTab = {};
    let roomsTab = {};


    if (id === "courses") {
        courseTab = document.getElementById("tab-courses");

        let conditions = courseTab.getElementsByClassName("form-group conditions")[0];

        let cond;
        if (conditions.getElementsByClassName("control-group condition-type")[0]
            .getElementsByTagName("input")[0]
            .getAttribute("checked") !== null) {
            cond = "AND";
        } else if (conditions.getElementsByClassName("control-group condition-type")[0]
            .getElementsByTagName("input")[1]
            .getAttribute("checked") !== null) {
            cond = "OR";
        } else if (conditions.getElementsByClassName("control-group condition-type")[0]
            .getElementsByTagName("input")[2]
            .getAttribute("checked") !== null) {
            cond = "NOT";
        }
        where = whereOBJHelper(conditions, cond, id);

        query["WHERE"] = where; // WHERE REQUIRED

        let options = {};
        options["COLUMNS"] = columnsOBJHelper(courseTab, id); // COLUMNS REQUIRED

        if (Object.keys(orderOBJHelper(courseTab, id)).length > 0) {

            options["ORDER"] = orderOBJHelper(courseTab, id); // ORDER NOT REQUIRED
        }

        query["OPTIONS"] = options; // OPTIONS REQUIRED

        let transformations = {"GROUP": {}, "APPLY": {}};
        if (Object.keys(groupOBJHelper(courseTab, id)).length > 0) {

            transformations["GROUP"] = groupOBJHelper(courseTab, id);
            transformations["APPLY"] = applyOBJHelper(courseTab, id);
            query["TRANSFORMATIONS"] = transformations;

        }


    } else if (id === "rooms") {
        roomsTab = document.getElementById("tab-rooms");

        let conditions = roomsTab.getElementsByClassName("form-group conditions")[0];

        let cond;
        if (conditions.getElementsByClassName("control-group condition-type")[0]
            .getElementsByTagName("input")[0]
            .getAttribute("checked") !== null) {

            cond = "AND";
        } else if (conditions.getElementsByClassName("control-group condition-type")[0]
            .getElementsByTagName("input")[1]
            .getAttribute("checked") !== null) {

            cond = "OR";
        } else if (conditions.getElementsByClassName("control-group condition-type")[0]
            .getElementsByTagName("input")[2]
            .getAttribute("checked") !== null) {

            cond = "NOT";
        }

        query["WHERE"] = whereOBJHelper(conditions, cond, id); // WHERE REQUIRED

        let options = { "COLUMNS": {}};
        options["COLUMNS"] = columnsOBJHelper(roomsTab, id); // COLUMNS REQUIRED

        if (Object.keys(orderOBJHelper(roomsTab, id)).length > 0) {

            options["ORDER"] = orderOBJHelper(roomsTab, id); // ORDER NOT REQUIRED
        }

        query["OPTIONS"] = options; // OPTIONS REQUIRED

        let transformations = {"GROUP": {}, "APPLY": {}};
        if (Object.keys(groupOBJHelper(roomsTab, id)).length > 0) {

            transformations["GROUP"] = groupOBJHelper(roomsTab, id);
            transformations["APPLY"] = applyOBJHelper(roomsTab, id);
            query["TRANSFORMATIONS"] = transformations;

        }
    }
    // bug
    // let tab = {};
    //
    // if (courseTab === document.getElementById("tab-courses")) {
    //     tab = courseTab;
    // } else {
    //     tab = roomsTab;
    // }

    return query;
};

function whereOBJHelper(conditions, cond, id) {
    let where = {};

    let filters = [];
    if (conditions.getElementsByClassName("conditions-container").length !== 0) {



        for (let eachControl of conditions.getElementsByClassName("conditions-container")[0]
            .getElementsByClassName("control-group condition")) {

            let not = eachControl.getElementsByTagName("input")[0].getAttribute("checked");



            let compareKey;

            // grab value = <<KEY>>
            for (let key of eachControl.getElementsByClassName("control fields")[0].getElementsByTagName("option")) {
                if (key.getAttribute("selected") !== null) {
                    compareKey = key.getAttribute("value"); // set compareKey to the value of key
                    break;
                }
            }

            let operation;

            for (let operator of eachControl.getElementsByClassName("control operators")[0].getElementsByTagName("option")) {
                if (operator.getAttribute("selected") !== null) {
                    operation = operator.getAttribute("value");
                    // operation = GT | LT | EQ | IS
                    break;
                }
            }
            let term;
            // let control = eachControl.getElementsByClassName("control term")[0]; // first Element Child;
            if (operation !== "IS") { // GT LT EQ
                term = Number(eachControl.getElementsByClassName("control term")[0].firstElementChild.getAttribute("value")); // turn into a number
            } else if (operation === "IS") {
                term = eachControl.getElementsByClassName("control term")[0].firstElementChild.getAttribute("value"); // leave comparator as string
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
            if (not !== "checked") { // if NOT box was checked
                filters.push(filter);

                // filter will be populated regularly
                // "WHERE": {
                //      "EQ": {
                //      courses_avg = 70;
                //        }
                //     }

            } else if (not === "checked") {
                filters.push({"NOT": filter});

                // "NOT": {
                //          "EQ": {} (FILTER)
                //        }
            }
        }

        const OR = {};

        if (filters.length > 1) {
            if (cond !== "NOT") {
                where[cond] = filters;
            } else if (cond === "NOT") {
                OR["OR"] = filters;
                where["NOT"] = OR;
            }
        } else if (filters.length === 1) {
            if (cond !== "NOT") {
                where = filters[0];
            } else if (cond === "NOT") {
                where["NOT"] = filters[0];
            }
        }

        return where;
    }
}

function columnsOBJHelper(tab, id) {
    let listCols = [];

    // for each COLUMN OPTION
    for (let val of tab.getElementsByClassName("form-group columns")[0].getElementsByClassName("control field")) {
        // for each val or option in form-groups
        if (val.firstElementChild.getAttribute("checked") !== null) {
            let key = id + "_" + val.firstElementChild.getAttribute("value");
            listCols.push(key);
        }
    }

    // for each TRANSFORMATION GROUP KEY
    for (let trans of tab.getElementsByClassName("form-group columns")[0].getElementsByClassName("control transformation")) {
        if (trans.firstElementChild.getAttribute("checked") !== null) {
            let key = trans.firstElementChild.getAttribute("value");
            listCols.push(key);
        }
    }

    return listCols;
}
function orderOBJHelper(tab, id) {
    let order = {};


    let keys = [];

    for (let option of tab.getElementsByClassName("form-group order")[0]
        .getElementsByTagName("select")[0]
        .getElementsByTagName("option")) {

        if (option.getAttribute("selected") !== null) {
            let key;
            // if (option.getAttribute("class") !== "transformation") {
            //     key = id + "_" + option.getAttribute("value");
            // } else {
            //     key = option.getAttribute("value");
            // }
            if (option.getAttribute("class") === null) {
                key = id + "_" + option.getAttribute("value");
            } else {
                key = option.getAttribute("value");
            }
            keys.push(key);
        }
    }

    if (keys.length === 0) {
        return order;
    }


    let dir;
    if (tab.getElementsByClassName("form-group order")[0]
        .getElementsByClassName("control descending")[0]
        .getElementsByTagName("input")[0]
        .getAttribute("checked") === "checked") {
        dir = "DOWN";
    } else {
        dir = "UP";
    }

    order["dir"] = dir;
    order["keys"] = keys;

    return order;
}
function groupOBJHelper(tab, id) {

    const keys = [];

    for (let val of  tab.getElementsByClassName("form-group groups")[0].getElementsByTagName("input")) {
        if (val.getAttribute("checked") !== null ){
            let key = id + "_" + val.getAttribute("value");
            keys.push(key);
        }
    }

    return keys;
}

function applyOBJHelper(tab, id) {
    // const transformations = tab.getElementsByClassName("form-group transformations")[0];
    const applys = [];

    for (let transformation of tab.getElementsByClassName("form-group transformations")[0]
        .getElementsByClassName("control-group transformation")) {

        // "maxSeats"
        let madeKey = transformation.getElementsByClassName("control term")[0].getElementsByTagName("input")[0].getAttribute("value");

        let operator;

        // operator = COUNT | MAX | AVG | MIN | SUM
        // transformation.getElementsByClassName("control operators")[0]
        //     .getElementsByTagName("option").find(function (operation) {
        //     if (operation.getAttribute("selected") !== null) {
        //         operator = operation.getAttribute("value");
        //     }
        // });

        for (let operation of transformation.getElementsByClassName("control operators")[0]
            .getElementsByTagName("option")) {

            if (operation.getAttribute("selected") !== null) {
                operator = operation.getAttribute("value");
            }
        }

        let key;
        // key = oneOf audit-avg-dept...
        // transformation.getElementsByClassName("control fields")[0]
        //     .getElementsByTagName("option").find(function (field) {
        //     if (field.getAttribute("selected") !== null) {
        //         key = field.getAttribute("value");
        //
        //     }
        // });

        for (let field of transformation.getElementsByClassName("control fields")[0]
            .getElementsByTagName("option")) {

            if (field.getAttribute("selected") !== null) {
                key = id + "_" + field.getAttribute("value");

            }
        }

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

    }

    return applys;
}
