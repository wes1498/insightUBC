/**
 * Created by rtholmes on 2016-06-19.
 */

import fs = require("fs");
import restify = require("restify");
import Log from "../Util";
import InsightFacade from "../controller/InsightFacade";
import {InsightDatasetKind} from "../controller/IInsightFacade";

/**
 * This configures the REST endpoints for the server.
 */
export default class Server {

    private port: number;
    private rest: restify.Server;

    constructor(port: number) {
        Log.info("Server::<init>( " + port + " )");
        this.port = port;
    }

    /**
     * Stops the server. Again returns a promise so we know when the connections have
     * actually been fully closed and the port has been released.
     *
     * @returns {Promise<boolean>}
     */
    public stop(): Promise<boolean> {
        Log.info("Server::close()");
        const that = this;
        return new Promise(function (fulfill) {
            that.rest.close(function () {
                fulfill(true);
            });
        });
    }

    /**
     * Starts the server. Returns a promise with a boolean value. Promises are used
     * here because starting the server takes some time and we want to know when it
     * is done (and if it worked).
     *
     * @returns {Promise<boolean>}
     */
    public start(): Promise<boolean> {
        const that = this;
        return new Promise(function (fulfill, reject) {
            try {
                Log.info("Server::start() - start");

                that.rest = restify.createServer({
                    name: "insightUBC",
                });
                that.rest.use(restify.bodyParser({mapFiles: true, mapParams: true}));
                that.rest.use(
                    function crossOrigin(req, res, next) {
                        res.header("Access-Control-Allow-Origin", "*");
                        res.header("Access-Control-Allow-Headers", "X-Requested-With");
                        return next();
                    });

                // This is an example endpoint that you can invoke by accessing this URL in your browser:
                // http://localhost:4321/echo/hello
                that.rest.get("/echo/:msg", Server.echo);
               // that.rest.get("/yeet", Server.getShit);

                that.rest.put("/dataset/:id/:kind", Server.addDatasetFiles);
                that.rest.del("/dataset/:id", Server.deleteDatasets);
                that.rest.post("/query", Server.postQuery);
                that.rest.get("/datasets", Server.getlistDataset);
                // Log.info("putted");

                // NOTE: your endpoints should go here

                // This must be the last endpoint!
                that.rest.get("/.*", Server.getStatic);

                that.rest.listen(that.port, function () {
                    Log.info("Server::start() - restify listening: " + that.rest.url);
                    fulfill(true);
                });

                that.rest.on("error", function (err: string) {
                    // catches errors in restify start; unusual syntax due to internal
                    // node not using normal exceptions here
                    Log.info("Server::start() - restify ERROR: " + err);
                    reject(err);
                });

            } catch (err) {
                Log.error("Server::start() - ERROR: " + err);
                reject(err);
            }
        });
    }

    private static getShit(req: restify.Request, res: restify.Response, next: restify.Next) {
        let result = Server.performEcho(req.params.msg);
        // console.log(result);
        // return "yeeeeeet";
        res.send("yeeeeeet");
        // return next();
    }

    // The next two methods handle the echo service.
    // These are almost certainly not the best place to put these, but are here for your reference.
    // By updating the Server.echo function pointer above, these methods can be easily moved.
    private static echo(req: restify.Request, res: restify.Response, next: restify.Next) {
        Log.trace("Server::echo(..) - params: " + JSON.stringify(req.params));
        try {
            // const response = Server.performEcho(req.params.msg);
            let response = "yeeeeeeeet mofocker";
            // console.log("made it here responds");
            Log.info("Server::echo(..) - responding " + 200);
            res.json(200, {result: "yeeeeeeet motherfucker"});
        } catch (err) {
            // console.log("made it here dosent respond");
            Log.error("Server::echo(..) - responding 400");
            res.json(400, {error: err});
        }
        return next();
    }

    private static performEcho(msg: string): string {
        if (typeof msg !== "undefined" && msg !== null) {
            // return `yeeeeeeeeeet`;
            return `${msg}...${msg}`;
        } else {
            return "Message not provided";
        }
    }

    private static getStatic(req: restify.Request, res: restify.Response, next: restify.Next) {
        const publicDir = "frontend/public/";
        Log.trace("RoutHandler::getStatic::" + req.url);
        let path = publicDir + "index.html";
        if (req.url !== "/") {
            path = publicDir + req.url.split("/").pop();
        }
        fs.readFile(path, function (err: Error, file: Buffer) {
            if (err) {
                res.send(500);
                Log.error(JSON.stringify(err));
                return next();
            }
            res.write(file);
            res.end();
            return next();
        });
    }
    private static addDatasetFiles(req: restify.Request, res: restify.Response, next: restify.Next) {
        Log.trace("Server::addDataset(..) - id: " + req.params.id + ", kind: " + req.params.kind);
        let datasetId = req.params.id;
        let datasetKind = req.params.kind;
        // console.log(datasetId);
        let x = "./test/data/" + datasetId + ".zip";
        let bitmap = fs.readFileSync(x);
        let datasetStr = new Buffer(bitmap).toString("base64");
        let instancefacade = InsightFacade.getInstance();
        let dataKind: InsightDatasetKind = InsightDatasetKind.Rooms;
        if (datasetKind === "Courses") {
            dataKind = InsightDatasetKind.Courses;
        } else {
            dataKind = InsightDatasetKind.Rooms;
        }
        instancefacade.addDataset(datasetId, datasetStr, dataKind).then((successResponse: any) => {
            fs.writeFile("data/" + datasetId + "tits" + ".json", JSON.stringify(successResponse), function (e) {
                // return reject(new InsightError("Error Saving Files " + e));
               // reject({code: 400, body: {error: "Error saving files"}});
            });
           // res.json(  successResponse.code + ": " + successResponse.body); // Formats json and res.sends
            res.json({code: 200, body: successResponse});
            return next();
        }).catch((failResponse: any) => {
            // res.json(failResonse.body);
            res.json({code: 400, body: failResponse});
            return next();
        });
    }
    private static deleteDatasets(req: restify.Request, res: restify.Response, next: restify.Next) {
        // console.log(process.cwd());
        let datasetId = req.params.id;
        // let datasetKind = req.params.kind;
        const instancefacade = InsightFacade.getInstance();
        instancefacade.removeDataset(datasetId).then((successResponse: any) => {
            res.json({code: 200, body: successResponse}); // Formats json and res.sends
            return next();
        }).catch((failResponse: any) => {
            // Log.error("Server::echo(..) - responding " + failResponse.code);
            // e.log(failResponse);
            let h = failResponse.message;
            res.json({code: 400, body: h});
           // res.send(failResponse);
            return next();
        });
    }
    private static postQuery(req: restify.Request, res: restify.Response, next: restify.Next) {
        let query = req.params.body;
        // let datasetKind = req.params.kind;
        const instancefacade = InsightFacade.getInstance();
        instancefacade.performQuery(query).then((successResponse: any) => {
            res.json({code: 200, body: successResponse}); // Formats json and res.sends
            return next();
        }).catch((failResponse: any) => {
            let h = failResponse.message;
            res.json({code: 400, body: h});
            return next();
        });
    }
    private static getlistDataset(req: restify.Request, res: restify.Response, next: restify.Next) {
        // let datasetKind = req.params.kind;
        const instancefacade = InsightFacade.getInstance();
        instancefacade.listDatasets().then((successResponse: any) => {
            res.send( successResponse); // Formats json and res.sends
            return next();
        });
    }

}
