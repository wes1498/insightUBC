import Server from "../src/rest/Server";

import InsightFacade from "../src/controller/InsightFacade";
import chai = require("chai");

import chaiHttp = require("chai-http");
import Log from "../src/Util";

describe("Facade D3", function () {

    let facade: InsightFacade = null;
    let server: Server = null;

    chai.use(chaiHttp);

    before(function () {
        facade = new InsightFacade();
        server = new Server(4321);
        // TODO: start server here once and handle errors properly
        server.start().then(() => {
            Log.info("Server starts successfully!");
        }).catch((err) => {
            Log.error("Error:" + err);
        });
    });

    after(function () {
        // TODO: stop server here once!
        server.stop().then(() => {
            Log.info("Server stops successfully!");
        }).catch((err) => {
            Log.error("Error:" + err);
        });
    });

    beforeEach(function () {
        Log.error("Errors in errors in errors");
        // might want to add some process logging here to keep track of what"s going on
    });

    afterEach(function () {
        // might want to add some process logging here to keep track of what"s going on
    });

    // TODO: read your courses and rooms datasets here once!

    // Hint on how to test PUT requests
    /*
    it("PUT test for courses dataset", function () {
        try {
            return chai.request(URL)
                .put(YOUR_PUT_URL)
                .attach("body", YOUR_COURSES_DATASET, COURSES_ZIP_FILENAME)
                .then(function (res: Response) {
                    // some logging here please!
                    expect(res.status).to.be.equal(204);
                })
                .catch(function (err) {
                    // some logging here please!
                    expect.fail();
                });
        } catch (err) {
            // and some more logging here!
        }
    });
    */

    // The other endpoints work similarly. You should be able to find all instructions at the chai-http documentation
});
