import Server from "../src/rest/Server";

import InsightFacade from "../src/controller/InsightFacade";
import chai = require("chai");

import chaiHttp = require("chai-http");
import {expect} from "chai";
import Log from "../src/Util";
import * as fs from "fs";

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
    it("list datasets", function () {
        return chai.request("http://localhost:4321")
            .get("/datasets")
            .then(function (res) {
                expect(res.status).to.equal(200);
            }).catch((err) => {
                expect.fail();
            });
    });
    it("put datasets rooms", function () {
        let files = process.cwd();
        let file2 = files + "/test/data/rooms.zip";
        return chai.request("http://localhost:4321")
            .put("/dataset/rooms/rooms")
            .attach("body", fs.readFileSync(file2), "rooms.zip")
            .then(function (res) {
                expect(res.status).to.equal(200);
            }).catch((err) => {
                expect.fail();
            });
    });
    it("put datasets courses", function () {
        let files = process.cwd();
        let file2 = files + "/test/data/courses.zip";
        return chai.request("http://localhost:4321")
            .put("/dataset/courses/courses")
            .attach("body", fs.readFileSync(file2), "courses.zip")
            .then(function (res) {
                // console.log(res);
                expect(res.status).to.equal(200);
            }).catch(() => {
                expect.fail();
            });
    });
    it("delete dataset courses", function () {
        return chai.request("http://localhost:4321")
            .del("/dataset/courses")
            .then(function (res) {
                expect(res.status).to.equal(200);
            }).catch(() => {
                expect.fail();
            });
    });
    it("delete dataset courses with 404", function () {
        return chai.request("http://localhost:4321")
            .del("/dataset/courses")
            .then(function () {
                expect.fail();
            }).catch((err) => {
                expect(err.status).to.equal(404);
            });
    });
    it("delete dataset courses with 400", function () {
        return chai.request("http://localhost:4321")
            .del("/dataset/").then(function () {
                expect.fail();
            }).catch((err) => {
                expect(err.status).to.equal(400);
            });
    });
    it("put datasets courses", function () {
        let files = process.cwd();
        let file2 = files + "/test/data/courses.zip";
        return chai.request("http://localhost:4321")
            .put("/dataset/courses/courses").attach("body", fs.readFileSync(file2), "courses.zip").then(function (res) {
                expect(res.status).to.equal(200);
            }).catch(() => {
                expect.fail();
            });
    });
    it("post 400", function () {
        let files = process.cwd();
        let file2 = files + "/test/queries/q2.json";
        let query = fs.readFileSync(file2).toString();
        // console.log(query);
        return chai.request("http://localhost:4321").post("/query").send(query).then(function (res) {
                // console.log(res);
                expect.fail();
            }).catch((err) => {
                // console.log(err);
            expect(err.status).to.equal(400);
            });
    });
    it("echo with 400", function () {
        return chai.request("http://localhost:4321")
            .get("/echo")
            .then(function () {
                expect.fail();
            }).catch((err) => {
                // console.log(err.status);
                expect(err.status).to.equal(500);
            });
    });
    it("echo with 200", function () {
        return chai.request("http://localhost:4321")
            .get("/echo/hello")
            .then(function (res) {
                expect(res.status).to.equal(200);
            }).catch((err) => {
                expect.fail();
            });
    });
    it("put datasets courses with 400", function () {
        let files = process.cwd();
        let file2 = files + "/test/data/courses.zip";
        return chai.request("http://localhost:4321")
            .put("/dataset/courses/rooms")
            .attach("body", fs.readFileSync(file2), "rooms.zip")
            .then(function () {
                expect.fail();
            }).catch((e) => {
                expect(e.status).to.equal(400);
            });
    });
    it("delete dataset rooms", function () {
        return chai.request("http://localhost:4321")
            .del("/dataset/rooms")
            .then(function (res) {
                expect(res.status).to.equal(200);
            }).catch(() => {
                expect.fail();
            });
    });
    it("delete dataset courses 2", function () {
        return chai.request("http://localhost:4321")
            .del("/dataset/courses")
            .then(function (res) {
                expect(res.status).to.equal(200);
            }).catch(() => {
                expect.fail();
            });
    });
    it("put post 200", function () {
        let files = process.cwd();
        let file2 = files + "/test/queries/q2.json";
        return chai.request("http://localhost:4321").post("/query")
            .attach("body", fs.readFileSync(file2), "q2.json")
            .then(function (res) {
                expect(res.status).to.equal(200);
            }).catch((e) => {
                expect.fail();
            });
    });
    it("load something", function () {
        return chai.request("http://localhost:4321")
            .get("/index.html")
            .then(function (res) {
                expect(res.status).to.equal(200);
            }).catch(() => {
                expect.fail();
            });
    });
    // The other endpoints work similarly. You should be able to find all instructions at the chai-http documentation
});
