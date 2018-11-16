/**
 * Receives a query object as parameter and sends it as Ajax request to the POST /query REST endpoint.
 *
 * @param query The query object
 * @returns {Promise} Promise that must be fulfilled if the Ajax request is successful and be rejected otherwise.
 */
CampusExplorer.sendQuery = function(query) {
    return new Promise(function(fulfill, reject) {
         let request = new XMLHttpRequest();
        request.open("POST", "/query", true);
        request.onreadystatechange = function() {
            if (this.readyState === 4 && this.status === 200) {
                fulfill(this.responseText);
            } else if (this.readyState === 4 && this.status === 400) {
                reject(this.responseText);
            }
        };
        request.send(query);

        console.log("sent");
    });
};
