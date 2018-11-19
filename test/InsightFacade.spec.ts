import "mocha";
import {expect} from "chai";

import {InsightDataset, InsightDatasetKind, InsightError, NotFoundError} from "../src/controller/IInsightFacade";
import InsightFacade from "../src/controller/InsightFacade";
import Log from "../src/Util";
import TestUtil from "./TestUtil";
import {log} from "util";

// This should match the JSON schema described in test/query.schema.json t
// except 'filename' which is injected when the file is read. works
export interface ITestQuery {
    title: string;
    query: any;  // make any to allow testing structurally invalid queries
    isQueryValid: boolean;
    result: string | string[];
    filename: string;  // This is injected when reading the file
}

describe("InsightFacade Add/Remove Dataset", function () {
    // Reference any datasets you've added to test/data here and they will
    // automatically be loaded in the Before All hook.
    // autobot commented on an earlier commit, try again ?
    const datasetsToLoad: { [id: string]: string } = {
        courses: "./test/data/courses.zip",
        coursesOne: "./test/data/courses_one.zip",
        coursesEmpty: "./test/data/courses_empty.zip",
        coursesHTML: "./test/data/courses_html.zip",
        coursesBlank: "./test/data/courses_blank.zip",
        coursesOnevalid: "./test/data/courses_onevalid.zip",
        coursesNofolder: "./test/data/courses_nofolder.zip",
        coursesWrong: "./test/data/courses_wrong.zip",
        coursesInvalid: "./test/data/courses_invalid.zip",
        coursesNoJS: "./test/data/courses_notjs.zip",
        coursesBroken: "./test/data/courses_broken.zip",
        coursesTwo: "./test/data/courses_two.zip",
        coursesDouble: "./test/data/courses_double.zip",
        rooms:  "./test/data/rooms.zip",
    };
    let insightFacade: InsightFacade;
    let datasets: { [id: string]: string };

    before(async function () {
        Log.test(`Before: ${this.test.parent.title}`);

        try {
            const loadDatasetPromises: Array<Promise<Buffer>> = [];
            for (const [id, path] of Object.entries(datasetsToLoad)) {
                loadDatasetPromises.push(TestUtil.readFileAsync(path));
            }
            const loadedDatasets = (await Promise.all(loadDatasetPromises)).map((buf, i) => {
                return { [Object.keys(datasetsToLoad)[i]]: buf.toString("base64") };
            });
            datasets = Object.assign({}, ...loadedDatasets);
            expect(Object.keys(datasets)).to.have.length.greaterThan(0);
        } catch (err) {
            expect.fail("", "", `Failed to read one or more datasets. ${JSON.stringify(err)}`);
        }

        try {
            insightFacade = new InsightFacade();
        } catch (err) {
            Log.error(err);
        } finally {
            expect(insightFacade).to.be.instanceOf(InsightFacade);
        }
    });

    beforeEach(function () {
        Log.test(`BeforeTest: ${this.currentTest.title}`);
    });

    after(function () {
        Log.test(`After: ${this.test.parent.title}`);
    });

    afterEach(function () {
        Log.test(`AfterTest: ${this.currentTest.title}`);
    });
    it("Should add rooms", async () => {
        const id: string = "rooms";
        let response: string[];

        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Rooms);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([id]);
        }
    });
    it("Should add courses", async () => {
        const id: string = "courses";
        let response: string[];

        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([id, "rooms"]);
        }
    });
    it("Should remove rooms dataset", async () => {
         const id: string = "rooms";
         let response: string;
         try {
             response = await insightFacade.removeDataset(id);
         } catch (err) {
             response = err;
         } finally {
             expect(response).to.equal(id);
         }
    });
    it("Should remove course dataset", async () => {
        const id: string = "courses";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });
    it("Should list 0 datasets before add uno", async () => {
        let response: InsightDataset[];

        try {
            response = await insightFacade.listDatasets();
        } catch (err) {
            expect.fail("Should not fail");
        }
        expect(response).to.have.lengthOf(0);
    });
    it("Should add a valid dataset", async () => {
        const id: string = "courses";
        let response: string[];

        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([id]);
        }
    });

    it("Should list 1 datasets", async () => {
        let response: InsightDataset[];

        try {
            response = await insightFacade.listDatasets();
        } catch (err) {
            expect.fail("Should not fail");
        }
        expect(response).to.have.lengthOf(1);
    });
    it("Should add a valid dataset dos tres", async () => {
        const id: string = "coursesTwo";
        let response: string[];

        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should list 2 datasets", async () => {
        let response: InsightDataset[];

        try {
            response = await insightFacade.listDatasets();
        } catch (err) {
            expect.fail("Should not fail");
        }
        expect(response).to.have.lengthOf(2);
    });
    it("Should add a valid dataset dos cuatro", async () => {
        const id: string = "coursesDouble";
        let response: string[];

        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should list 3 datasets", async () => {
        let response: InsightDataset[];

        try {
            response = await insightFacade.listDatasets();
        } catch (err) {
            expect.fail("Should not fail");
        }
        expect(response).to.have.lengthOf(3);
    });
    it("Should remove the courses dataset uno", async () => {
        const id: string = "courses";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });
    it("Should list 1 datasets before add uno", async () => {
        let response: InsightDataset[];

        try {
            response = await insightFacade.listDatasets();
        } catch (err) {
            expect.fail("Should not fail");
        }
        expect(response).to.have.lengthOf(2);
    });
    it("Should add a valid dataset dos tres", async () => {
        const id: string = "courses";
        let response: string[];

        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should list 1 datasets before add uno courses same", async () => {
        let response: InsightDataset[];

        try {
            response = await insightFacade.listDatasets();
        } catch (err) {
            expect.fail("Should not fail");
        }
        expect(response).to.have.lengthOf(3);
    });
    it("Should remove the courses dataset uno", async () => {
        const id: string = "courses";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });
    it("Should get a not found error removing twice", async () => {
        const id: string = "courses";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(NotFoundError);
        }
    });
    it("Should list 0 datasets after remove 1 last", async () => {
        let response: InsightDataset[];

        try {
            response = await insightFacade.listDatasets();
        } catch (err) {
            expect.fail("Should not fail");
        }
        expect(response).to.have.lengthOf(0);
    });
    it("Should list 0 datasets before add", async () => {
        let response: InsightDataset[];

        try {
            response = await insightFacade.listDatasets();
        } catch (err) {
            expect.fail("Should not fail");
        }
        expect(response).to.have.lengthOf(0);
    });
    it("Should not remove when dataset was not in", async () => {
        const id: string = "courses";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(NotFoundError);
        }
    });
    // Add dataset
    it("Should add a valid dataset", async () => {
        const id: string = "courses";
        let response: string[];

        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([id]);
        }
    });
    it("Should list 1 datasets after add", async () => {
        let response: InsightDataset[];

        try {
            response = await insightFacade.listDatasets();
        } catch (err) {
            expect.fail("Should not fail");
        } finally {
            expect(response).to.be.lengthOf(1);
        }
    });
    it("Should add another valid dataset", async () => {
        const id: string = "coursesNoJS";
        let response: string[];

        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should list 2 datasets after", async () => {
        let response: InsightDataset[];

        try {
            response = await insightFacade.listDatasets();
        } catch (err) {
            expect.fail("Should not fail");
        } finally {
            expect(response).to.be.lengthOf(2);
        }
    });
    it("Should not add an empty dataset", async () => {
        const id: string = "coursesEmpty";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should not add wrong dataset", async () => {
        const id: string = "coursesWrong";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should not add an empty string", async () => {
        const id: string = "";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should not add an no json string", async () => {
        const id: string = "courses";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should not add an empty empty string", async () => {
        const id: string = "";
        let response: string[];
        try {
            response = await insightFacade.addDataset("", datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should not add an empty empty empty string", async () => {
        const id: string = "";
        let response: string[];
        try {
            response = await insightFacade.addDataset("", "", InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should not add null null null", async () => {
        const id: string = null;
        let response: string[];
        try {
            response = await insightFacade.addDataset(null, null, null);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should not add null", async () => {
        const id: string = null;
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should not add invalid", async () => {
        const id: string = "coursesInvalid";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should not add not added ", async () => {
        const id: string = "coursesHello";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should not add null null", async () => {
        const id: string = null;
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, null, InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should not add undefined undefined undefined", async () => {
        const id: string = undefined;
        let response: string[];
        try {
            response = await insightFacade.addDataset(undefined, undefined, undefined);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should not add undefined ", async () => {
        const id: string = undefined;
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should not add undefined undefined", async () => {
        const id: string = undefined;
        let response: string[];
        try {
            response = await insightFacade.addDataset(undefined, undefined, InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should not add a blank dataset", async () => {
        const id: string = "coursesBlank";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should add the valid dataset from invalid sets ", async () => {
        const id: string = "coursesOnevalid";
        let response: string[];

        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            // expect(response).to.deep.equal([id]);
            expect(response).to.contain("coursesOnevalid");
        }
    });

    it("Should not add an invalid dataset (html)", async () => {
        const id: string = "coursesHTML";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("should not update a valid dataset", async () => {
        const id: string = "courses";
        let response: string[];

        try {
            await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal([id]);
        }
    });
    it("should not add if no courses", async () => {
        const id: string = "coursesNofolder";
        let response: string[];

        try {
            await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal([id]);
        }
    });
    // This is an example of a pending test. Add a callback function to make the test run.
    it("Should remove the courses dataset", async () => {
        const id: string = "courses";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });
    it("Should list 2 datasets after", async () => {
        let response: InsightDataset[];

        try {
            response = await insightFacade.listDatasets();
        } catch (err) {
            expect.fail("Should not fail");
        } finally {
            expect(response).to.be.lengthOf(1);
        }
    });
    it("Should remove the blank dataset", async () => {
        const id: string = "coursesBlank";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });
    it("Should not remove empty string", async () => {
        const id: string = "";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(NotFoundError);
        }
    });
    it("Should not remove not added", async () => {
        const id: string = "coursesHello";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(NotFoundError);
        }
    });
    it("Should not remove null string", async () => {
        const id: string = null;
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should not remove invalid", async () => {
        const id: string = "coursesInvalid";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should not remove undef string", async () => {
        const id: string = undefined;
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });
    it("Should add a valid bool", async () => {
         const id: boolean = false;
         let response: string[];
         try {
             response = await insightFacade.addDataset("", "", InsightDatasetKind.Courses);
         } catch (err) {
             response = err;
         } finally {
             expect(response).to.be.instanceOf(InsightFacade);
         }
     });
    it("Should add a valid bool", async () => {
         const id: boolean = true;
         let response: string[];
         try {
             response = await insightFacade.addDataset("", "", InsightDatasetKind.Courses);
         } catch (err) {
             response = err;
         } finally {
             expect(response).to.be.instanceOf(InsightFacade);
         }
     });
    it("Should not add a valid broken", async () => {
        const id: string = "coursesBroken";
        let response: string[];

        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightFacade);
        }
    });
    it("Should not remove invalid", async () => {
        const id: string = "coursesBroken";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("Should add test smaller sets ", async () => {
        const id: string = "coursesOne";
        let response: string[];

        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            // expect(response).to.deep.equal([id]);
            expect(response).to.contain("coursesOne");
        }
    });
    it("Should remove the courses dataset uno", async () => {
        const id: string = "courses";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });
    it("Should remove the courses dataset uno", async () => {
        const id: string = "coursesOnevalid";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });
    it("Should remove the courses dataset uno", async () => {
        const id: string = "coursesOne";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });
});

// This test suite dynamically generates tests from the JSONe files in test/queries.
// You should not need to modify it; instead, add additional files to the queries directory. ready
describe("InsightFacade PerformQuery", () => {
    const datasetsQuery: { [id: string]: string } = {
        courses: "./test/data/courses.zip",
        rooms: "./test/data/rooms.zip",
    };
    let insightFacade: InsightFacade;
    let testQueries: ITestQuery[] = [];

    // Create a new instance of InsightFacade, read in the test queries from test/queries and
    // add the datasets specified in datasetsQuery.
    before(async function () {
        Log.test(`Before: ${this.test.parent.title}`);

        // Load the query JSON files under test/queries.
        // Fail if there is a problem reading ANY query.
        try {
            testQueries = await TestUtil.readTestQueries();
            expect(testQueries).to.have.length.greaterThan(0);
        } catch (err) {
            expect.fail("", "", `Failed to read one or more test queries. ${JSON.stringify(err)}`);
        }

        try {
            insightFacade = new InsightFacade();
        } catch (err) {
            Log.error(err);
        } finally {
            expect(insightFacade).to.be.instanceOf(InsightFacade);
        }

        // Load the datasets specified in datasetsQuery and add them to InsightFacade. r
        // Fail if there is a problem reading ANY dataset.
        try {
            const loadDatasetPromises: Array<Promise<Buffer>> = [];
            for (const [id, path] of Object.entries(datasetsQuery)) {
                loadDatasetPromises.push(TestUtil.readFileAsync(path));
            }
            const loadedDatasets = (await Promise.all(loadDatasetPromises)).map((buf, i) => {
                return { [Object.keys(datasetsQuery)[i]]: buf.toString("base64") };
            });
            expect(loadedDatasets).to.have.length.greaterThan(0);

            const responsePromises: Array<Promise<string[]>> = [];
            const datasets: { [id: string]: string } = Object.assign({}, ...loadedDatasets);
            // for (const [id, content] of Object.entries(datasets)) {
            //     responsePromises.push(insightFacade.addDataset(id, content, InsightDatasetKind.Courses));
            // }
            responsePromises.push(
                insightFacade.addDataset("courses", datasets["courses"], InsightDatasetKind.Courses));
            responsePromises.push(
                insightFacade.addDataset("rooms", datasets["rooms"], InsightDatasetKind.Rooms));

            // This try/catch is a hack to let your dynamic tests execute even if the addDataset method fails.
            // In D1, you should remove this try/catch to ensure your datasets load successfully before trying
            // to run you queries.
            try {
                const responses: string[][] = await Promise.all(responsePromises);
                responses.forEach((response) => expect(response).to.be.an("array"));
            } catch (err) {
                Log.warn(`Ignoring addDataset errors. For D1, you should allow errors to fail the Before All hook.`);
            }
        } catch (err) {
            expect.fail("", "", `Failed to read one or more datasets. ${JSON.stringify(err)}`);
        }
    });

    beforeEach(function () {
        Log.test(`BeforeTest: ${this.currentTest.title}`);
    });

    after(function () {
        Log.test(`After: ${this.test.parent.title}`);
    });

    afterEach(function () {
        Log.test(`AfterTest: ${this.currentTest.title}`);
    });

    // Dynamically create and run a test for each query in testQueries
    it("Should run test queries", () => {
        describe("Dynamic InsightFacade PerformQuery tests", () => {
            for (const test of testQueries) {
                it(`[${test.filename}] ${test.title}`, async () => {
                    let response: any[];

                    try {
                        response = await insightFacade.performQuery(test.query);
                    } catch (err) {
                        response = err;
                    } finally {
                        if (test.isQueryValid) {
                            expect(response).to.deep.equal(test.result);
                        } else {
                            expect(response).to.be.instanceOf(InsightError);
                        }
                    }
                });
            }
        });
    });
});
