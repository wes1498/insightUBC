import Log from "../Util";
import {IInsightFacade, InsightDataset, InsightDatasetKind, InsightCourse} from "./IInsightFacade";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */

const JSZip = require("jszip");
const COURSES = InsightDatasetKind.Courses;
const ROOMS = InsightDatasetKind.Rooms;
const coursesMap: Map<string, InsightCourse[]> = new Map<string, InsightCourse[]>();
export default class InsightFacade implements IInsightFacade {

    public loadDatasets: Map<string, any[]>;

    public static insightFacade: InsightFacade = new InsightFacade();

    constructor() {
        Log.trace("InsightFacadeImpl::init()");
        this.loadDatasets = new Map<string, any[]>();
    }
    public static getInstance() {
        return InsightFacade.insightFacade;
    }

    public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
        return new Promise(function (resolve, reject) {
            let isDatasetLoaded = false;

            if (this.loadDatasets.has(id)) {
                isDatasetLoaded = true;
            }
            let zip = new JSZip();
            zip.loadAsync(content).then(function (newZip: any) {
                Object.keys(zip.files).forEach(function (filename) {
                    newZip.files[filename].async("string").then(function (fileData: any) {
                       console.log(filename);
                    });
                });
            });
            if (kind === InsightDatasetKind.Courses || this.loadedDatasets.has(id)) {
                this.coursesHelper(id, content, isDatasetLoaded);
            } else {
                reject("error occured in handling dataset");
            }
        });
    }

    // public  coursesHelper(id: string, content: string, isloaded: boolean);

    public removeDataset(id: string): Promise<string> {
        return Promise.reject("Not implemented.");
    }

    public performQuery(query: any): Promise <any[]> {
        return Promise.reject("Not implemented.");
    }

    public listDatasets(): Promise<InsightDataset[]> {
        return new Promise<InsightDataset[]>((fulfill, reject) => {
            const result: InsightDataset[] = [];
            for (const id of coursesMap.keys()) {
                const numRows = coursesMap.get(id).length;
                result.push({id, kind: COURSES, numRows});
            }
            const body: InsightDataset[] = result;
            fulfill();
        });
    }
}
