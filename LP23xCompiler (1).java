import sbnf.ParseException;
import sbnf.lex.Lexer;
import sbnf.lex.Token;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.*;

public class LP23xCompiler {
    private List<String> emitted;
    private final String sbnfFilePath;
    private Lexer lex;
    private int freshNameCounter;
    private final PrintStream out;
    boolean array = false;
    boolean rtn = false;
    String offset = "";
    boolean isfun = false;
    String type = "";
    boolean malloc = false;

    String id = freshName();
    String lexp = "";
    String ret = freshName("ret");
    ArrayList<String> functioname = new ArrayList<>();

    ArrayList<String> endlist = new ArrayList<>();
    int parametercount = 1;
    int parametersdefcount = 2;
    int localCount = 0;

    private Map<String, Integer> globalVars = new HashMap<>();
    private Map<String, Integer> globalVars2 = new HashMap<>();
    private Map<String, Integer> localVars = new HashMap<>();
    private Map<String, Integer> parameters = new HashMap<>();
    private Map<String, String> idMap = new HashMap<>();
    int[] arr;
    String mainglobaltest = freshName("teest");
    /**
     * Initialise a new compiler.
     *
     * @param sbnfFilePath path to SBNF file containing the required token definitions,
     * @param out          generated SSM assembly code will be written here.
     */
    public LP23xCompiler(String sbnfFilePath, PrintStream out) {
        this.sbnfFilePath = sbnfFilePath;
        this.out = out;
    }

    /**
     * Transform an LP23x identifier into a name which is guaranteed
     * not to clash with any of the SSM assembly instruction names.
     * Since LP23x identifiers cannot start with a $-sign, all we
     * need do is prefix the name with a $-sign.
     *
     * @param sourceName
     * @return
     */
    private static String makeSafe(String sourceName) {
        assert !sourceName.startsWith("$");
        return "$" + sourceName;
    }

    /**
     * Each call to this method will return a fresh name which is
     * guaranteed not to clash with any SSM assembly instruction
     * names or with any name returned by makeSafe(x) where x is
     * an LP23x identifier.
     *
     * @param prefix a string to include as part of the generated name.
     * @return a fresh name.
     */
    private String freshName(String prefix) {
        return "$$" + prefix + "_" + (freshNameCounter++);
    }

    /**
     * Each call to this method will return a fresh name which is
     * guaranteed not to clash with any SSM assembly instruction
     * names or with any name returned by makeSafe(x) where x is
     * an LP23x identifier.
     *
     * @return a fresh name.
     */
    private String freshName() {
        return freshName("");
    }

    /**
     * Emit a sequence of SSM assembly instructions.
     *
     * @param ss the instructions.
     */
    private void emit(String... ss) {
        for (String s : ss) emitted.add("\t" + s);
    }

    /**
     * Emit an SSM assembly label.
     *
     * @param label the label name.
     */
    private void emitLabel(String label) {
        emitted.add(label + ":");
    }

    /**
     * Compile an LP23x program to SSM assembly code.
     *
     * @param filePath the path to the LP23x source file.
     * @throws IOException
     */
    public void compile(String filePath) throws IOException {
        // add additional initialisation here if required by your compiler
        emitted = new ArrayList<>();
        freshNameCounter = 0;
        lex = new Lexer(sbnfFilePath);
        lex.readFile(filePath);
        lex.next();
        Prog();
        // initialisation of statically allocated variables (the globals)
        emitted.add(".data");
        for (String varName : globalVars.keySet()) {
            emitted.add(varName + ": " + globalVars.get(varName));
        }

        for (String varName : globalVars2.keySet()) {
            emitted.add(varName + ": " + globalVars2.get(varName));
        }
        // To be completed

        // this writes your emitted code to the output file,
        // don't mess with it!
        emitted.stream().forEach(out::println);
    }

    private void exp() {
        simpleExp();
        operatorClause();
    }

    private void simpleExp() {
        switch (lex.tok().type) {
            case "INT":
                String n = lex.tok().image;
                lex.next();

                emit("push " + n);
                break;

            case "MALLOC":
                lex.next();
                lex.eat("LSQBR");

                exp();

                malloc = true;
               // mainglobaltest = freshName("teest");
                globalVars2.put(mainglobaltest,0);
                emit("push " + mainglobaltest);
                emit("store");

                emit("get_lp");

                lex.eat("RSQBR");


                break;
            case "ID":
                expIdFollow();
                break;

            case "LBR":
                lex.next();
                exp();
                lex.eat("RBR");
                break;

            case "SUB":
                lex.next();
                emit("push 0");
                simpleExp();
                emit("sub");
                break;

            case "PP":
                lex.next();
                lexp();

                //lex.next();
                // optionalIndexer(lexp);
                break;

            case "NOT":
                lex.next();
                simpleExp();
                emit("test_z");
                break;
        }
    }

    private void operator() throws ParseException {

        switch (lex.tok().type) {
            case "MUL":
                lex.next();
                simpleExp();
                emit("mul");
                break;

            case "DIV":
                lex.next();
                simpleExp();
                emit("dup");
                String zeroDivLabel = freshName("zeroDiv");
                String main = freshName("main");
                emit("push " + zeroDivLabel);
                emit("jump_z");
                emit("div");
                emit("push " + main);
                emit("jump");
                emitLabel(zeroDivLabel);
                emit("push 0");
                emitLabel(main);
                break;

                /*
                lex.next();
                simpleExp();

                emit("push $eldo");
                emit("jump_z");
                emit("push 1"); // push default value onto stack
                emit("swap"); // swap default value with top of stack
                emit("div"); // perform division


                 */


            case "SUB":
                lex.next();
                simpleExp();
                emit("sub");
                break;

            case "ADD":
                lex.next();
                simpleExp();
                emit("add");
                break;
            case "EQ":
                lex.next();          // consume the operator token
                simpleExp();         // evaluate the left-hand side
                emit("swap");        // swap the top two values on the stack
                simpleExp();         // evaluate the right-hand side
                emit("sub");         // subtract the two values
                emit("test_z");      // test if the result is zero
                break;

            case "LE":
                lex.next();
                simpleExp();
                emit("swap"); // move the left operand to the top of the stack
                emit("sub");  // subtract the left operand from the right operand
                emit("test_n"); // test if the result is negative or zero
                emit("test_z");
                break;

            case "OR":
                String orShortCircuit = freshName("or");
                String orShortCircuit1 = freshName("or1");
                String orShortCircuit2 = freshName("or2");
                lex.next();
                emit("test_z");
                emit("push " + orShortCircuit1);
                emit("jump_z");
                emit("push 1");
                emit("push " + orShortCircuit);
                emit("jump");

                emitLabel(orShortCircuit);
                simpleExp();
                emit("test_z");
                emit("test_z");
                emit("mul");


                emit("push " + orShortCircuit2);
                emit("jump");
                emitLabel(orShortCircuit1);
                emit("push 1");
                emitLabel(orShortCircuit2);
                break;

            case "LT":
                lex.next();
                simpleExp();
                emit("sub");
                emit("test_n");
                break;

            case "AND":
                String andShortCircuit = freshName("and");
                String andShortCircuit1 = freshName("and1");
                String andShortCircuit2 = freshName("and2");
                lex.next();

                emit("push " + andShortCircuit1);
                emit("jump_z");
                emit("push 1");
                emit("push " + andShortCircuit);
                emit("jump");

                emitLabel(andShortCircuit);
                simpleExp();

                emit("mul");
                emit("test_z");
                emit("test_z");
                emit("push " + andShortCircuit2);
                emit("jump");

                emitLabel(andShortCircuit1);
                emit("push 0");
                emitLabel(andShortCircuit2);
//
                break;

            default:

        }
    }

    private void operatorClause() {
        operator();
        simpleExp();
    }


    private boolean isExp(String type) {
        switch (type) {
            case "INT":
            case "ID":
            case "LBR":
            case "SUB":
            case "NOT":
            case "PP":
            case "MALLOC":
                return true;
            default:
                return false;
        }
    }

    private void actuals() {
        if (isExp(lex.tok().type)) {
            parametercount++;
            exp();
            actualsRest();
        }

    }

    private void actualsRest() {
        if (lex.tok().type.equals("COMMA")) {
            lex.eat("COMMA");
            parametercount++;
            exp();
            actualsRest();
        }
    }


    private void expIdFollow() {
        String na = lex.tok().image;
        lex.next();

        if (lex.tok().type.equals("LBR")) {

            lex.eat("LBR");
            //
            // parameters = new HashMap<>();
            ret = freshName("ret");

            if (!functioname.contains(na)) {
                id = freshName();
                functioname.add(na);

            } else {
                parametercount--;
            }


            if (!idMap.containsKey(na)) {
                idMap.put(na, id);
            }

            if (idMap.containsKey(na)) {
                emit("push " + idMap.get(na));

            }

            emit("push " + ret);
            parametercount = 1;
            actuals();
            if (parametercount == 0) {
                parametercount = 1;
            }


            emit("push " + parametercount);
            emit("call");

            emitLabel(ret);


            lex.eat("RBR");

        } else {

            variables(na);
            //load appropriate global, local, parameter.
            //global

        }
    }

    public void indexer() {

    }

    private void variables(String na) {


        if (globalVars.containsKey(na) && !localVars.containsKey(na) && !parameters.containsKey(na)) {
            emit("push " + na);
            emit("load");
            if (lex.tok().type.equals("LSQBR")) {
                lex.next();
                exp();
                emit("add");
                emit("load");
                lex.next();
            }


        } else if (parameters.containsKey(na)) {
            int index = parameters.get(na);
            emit("get_fp");
            emit("push " + index);
            emit("add");
            emit("load");
            if (lex.tok().type.equals("LSQBR")) {
                lex.next();
                exp();
                emit("add");
                emit("load");
                lex.next();
            }
        } else if (localVars.containsKey(na)) {

            emit("get_fp");
            emit("push " + localVars.get(na));
            emit("sub");
            emit("load");
            if (lex.tok().type.equals("LSQBR")) {
                lex.next();
                exp();
                emit("add");
                emit("load");
                lex.next();
            }
        }

    }

    private void globals() {
        if (lex.tok().type.equals("GLOBALS")) {
            lex.next();
            String varName = lex.tok().image;
            lex.eat("ID");
            globalVars.put(varName, 0);
            varListRest("g");
            lex.eat("SEMIC");
        }
    }


    private void loadvariable(String varNam, String types) {
        if (types.equals("global")) {

            int value;

            exp();//get_lp



            emit("push " + varNam);

            if (array) {
                emit("load");
                if (type.equals("ID"))
                    variables(offset);
                else {
                    emit("push " + offset);
                }
                emit("add");
            }
            emit("store");


        if(malloc) {
            String start = freshName("initial");

            String end = freshName("initiale");
            emitLabel(start);

            emit("push 0");
            emit("get_lp");
            emit("push " + mainglobaltest);
            emit("load");
            emit("add");
            emit("store");

            emit("push " + mainglobaltest);
            emit("load");
            emit("push 1");
            emit("sub");


            emit("push " + mainglobaltest);
            emit("store");

            emit("push " + mainglobaltest);
            emit("load");


            emit("push " + end);
            emit("jump_n");
            emit("push " + mainglobaltest);
            emit("load");

            emit("push " + start);
            emit("jump");

            emitLabel(end);

        }
        malloc =false;
            emit("push " + varNam);
            emit("load");
            array = false;
        }

    }

    private void idFollow() {

        String varName = lex.tok().image;
        lex.next();
        if (lex.tok().type.equals("LSQBR")) {
            lex.eat("LSQBR");
            type = lex.tok().type;
            offset = lex.tok().image;
            // variables(varName);
            array = true;
            exp();
            lex.eat("RSQBR");
        }

//lex.mext();

        if (lex.tok().type.equals("ASSIGN")) {
            lex.eat("ASSIGN");


            //if both parameter and local are empty + global is defined we can load global


            if (globalVars.containsKey(varName) && !localVars.containsKey(varName) && !parameters.containsKey(varName)) { //add local
                loadvariable(varName, "global");
            }

            // <- parameter exp
            if (parameters.containsKey(varName)) {
                String copyparameter = lex.tok().image;
                exp();
                emit("get_fp");
                emit("push " + parameters.get(varName));
                emit("add");

                if (array) {
                    emit("load");
                    if (type.equals("ID"))
                        variables(offset);
                    else {
                        emit("push " + offset);
                    }
                    emit("add");
                }
                array = false;
                emit("store");
                if(malloc) {
                    String start = freshName("initiala");

                    String end = freshName("initialea");
                    emitLabel(start);

                    emit("push 0");
                    emit("get_lp");
                    emit("push " + mainglobaltest);
                    emit("load");
                    emit("add");
                    emit("store");

                    emit("push " + mainglobaltest);
                    emit("load");
                    emit("push 1");
                    emit("sub");


                    emit("push " + mainglobaltest);
                    emit("store");

                    emit("push " + mainglobaltest);
                    emit("load");


                    emit("push " + end);
                    emit("jump_n");
                    emit("push " + mainglobaltest);
                    emit("load");

                    emit("push " + start);
                    emit("jump");

                    emitLabel(end);

                }
                malloc =false;

            }

            if (localVars.containsKey(varName)) {
                //
                exp();
                emit("get_fp");
                emit("push " + localVars.get(varName));
                emit("sub");

                if (array) {
                    emit("load");
                    if (type.equals("ID"))
                        variables(offset);
                    else {
                        emit("push " + offset);
                    }
                    emit("add");
                }
                array = false;
                emit("store");
                if(malloc) {
                    String start = freshName("initialaa");

                    String end = freshName("initialeaa");
                    emitLabel(start);

                    emit("push 0");
                    emit("get_lp");
                    emit("push " + mainglobaltest);
                    emit("load");
                    emit("add");
                    emit("store");

                    emit("push " + mainglobaltest);
                    emit("load");
                    emit("push 1");
                    emit("sub");


                    emit("push " + mainglobaltest);
                    emit("store");

                    emit("push " + mainglobaltest);
                    emit("load");


                    emit("push " + end);
                    emit("jump_n");
                    emit("push " + mainglobaltest);
                    emit("load");

                    emit("push " + start);
                    emit("jump");

                    emitLabel(end);

                }
                malloc =false;
            }


            lex.eat("SEMIC");
        }

        if (lex.tok().type.equals("LBR")) {
            parametercount = 1;
            String ret = freshName("ret");
            lex.eat("LBR");

            if (!idMap.containsKey(varName)) {
                id = freshName();
                idMap.put(varName, id);

            }
            emit("push " + idMap.get(varName));
            emit("push " + ret);
            actuals();
            emit("push " + parametercount);
            emit("call");

            emitLabel(ret);
            lex.eat("RBR");
            lex.eat("SEMIC");

        }

    }

    private void alt() {
        if (lex.tok().type.equals("ELIF")) {
            String iff = freshName("if");
            String eldo = freshName("eldo");
            String end = freshName("end");

            lex.next();
            exp();

            emit("push " + eldo);
            emit("jump_z");
            emit("push " + iff);
            emit("jump");

            freshName();
            emitLabel(iff);

            lex.eat("DO");

            while (isStm(lex.tok().type)) {

                stm();
            }

            emit("push " + end);
            emit("jump");

            emitLabel(eldo);
            alt();
            emitLabel(end);
        } else if (lex.tok().type.equals("ELDO")) {
            lex.next();
            while (isStm(lex.tok().type)) {
                stm();
            }
            lex.eat("ENDIF");
        } else if (lex.tok().type.equals("ENDIF")) {
            lex.next();
        }

    }

    private void lexp() {
        lexp = lex.tok().image;

        if (lex.tok().type.equals("ID")) {

            variables(lexp);
            //
            lex.eat("ID");
            if (!lex.tok().type.equals("LSQBR")) {
                if (globalVars.containsKey(lexp) && !localVars.containsKey(lexp) && !parameters.containsKey(lexp)) { //add local
                    emit("push 1");
                    emit("add");
                    emit("push " + lexp);
                    emit("store");
                    variables(lexp);


                }

                // <- parameter exp
                if (parameters.containsKey(lexp)) {
                    String copyparameter = lex.tok().image;
                    emit("push 1");
                    emit("add");
                    emit("get_fp");
                    emit("push " + parameters.get(lexp));
                    emit("add");

                    emit("store");
                    variables(lexp);
                }

                if (localVars.containsKey(lexp)) {
                    //

                    emit("push 1");
                    emit("add");
                    emit("get_fp");
                    emit("push " + localVars.get(lexp));
                    emit("sub");
                    emit("store");
                    variables(lexp);
                }

            }
            optionalIndexer(lexp);
        }


    }

    private void optionalIndexer(String id) {
        Lexer a = lex;
        boolean arr = false;
        String off = "";
        //remember do some testin here then iasjdkalsjd
        //lex.next();
        if (lex.tok().type.equals("LSQBR")) {
            lexp = id;
            lex.eat("LSQBR");
            String types = lex.tok().type;
            off = lex.tok().image;
            // variables(varName);
            arr = true;


            variables(lexp);
            lex.next();
            //load the address from variable , address of array starting location
            if (globalVars.containsKey(lexp) && !localVars.containsKey(lexp) && !parameters.containsKey(lexp)) { //add local
                if (types.equals("INT")) {
                    //load the value at that position
                    emit("push " + off);
                    emit("add");
                    emit("load");

                    //add one to overall
                    emit("push 1");
                    emit("add");

                    //load it again and store it
                    variables(lexp);
                    emit("push " + off);
                    emit("add");
                    emit("store");
                    //load the value for sysc to print
                    variables(lexp);
                    emit("push " + off);
                    emit("add");
                    emit("load");
                } else {
                    //load custom array index variable
                    variables(off);
                    emit("add");
                    emit("load");

                    //add one to array index
                    emit("push 1");
                    emit("add");

                    //load value  custom variable to add , load index
                    variables(lexp);
                    variables(off);
                    emit("add");
                    emit("store");
                    //load it for printing.
                    variables(lexp);
                    emit("push " + off);
                    emit("add");
                    emit("load");
                }
            }

            // <- parameter exp
            if (parameters.containsKey(lexp)) {
                if (types.equals("INT")) {
                    //load the value at that position
                    emit("push " + off);
                    emit("add");
                    emit("load");

                    //add one to overall
                    emit("push 1");
                    emit("add");

                    //load it again and store it
                    variables(lexp);
                    emit("push " + off);
                    emit("add");
                    emit("store");
                    //load the value for sysc to print
                    variables(lexp);
                    emit("push " + off);
                    emit("add");
                    emit("load");
                } else {
                    //load custom array index variable
                    variables(off);
                    emit("add");
                    emit("load");

                    //add one to array index
                    emit("push 1");
                    emit("add");

                    //load value  custom variable to add , load index
                    variables(lexp);
                    variables(off);
                    emit("add");
                    emit("store");
                    //load it for printing.
                    variables(lexp);
                    emit("push " + off);
                    emit("add");
                    emit("load");
                }
            }


            if (localVars.containsKey(lexp)) {
                //
                if (types.equals("INT")) {
                    //load the value at that position
                    emit("push " + off);
                    emit("add");
                    emit("load");

                    //add one to overall
                    emit("push 1");
                    emit("add");

                    //load it again and store it
                    variables(lexp);
                    emit("push " + off);
                    emit("add");
                    emit("store");
                    //load the value for sysc to print
                    variables(lexp);
                    emit("push " + off);
                    emit("add");
                    emit("load");
                } else {
                    //load custom array index variable
                    variables(off);
                    emit("add");
                    emit("load");

                    //add one to array index
                    emit("push 1");
                    emit("add");

                    //load value  custom variable to add , load index
                    variables(lexp);
                    variables(off);
                    emit("add");
                    emit("store");
                    //load it for printing.
                    variables(lexp);
                    emit("push " + off);
                    emit("add");
                    emit("load");
                }
            }

            lex.eat("RSQBR");
            // generate code to access array element
        } else {
            // generate code to access variable
        }
    }

    private void stm() throws ParseException {
        boolean executedIf = false;
        switch (lex.tok().type) {

            case "ID":

                //id = freshName("foo");
                idFollow();
                break;


            case "PP":
                lex.eat("PP");
                lexp();
                lex.eat("SEMIC");
                break;

            case "IF":

                String iff = freshName("if");
                String eldo = freshName("eldo");
                String end = freshName("end");

                lex.next();
                exp();

                emit("push " + eldo);
                emit("jump_z");
                emit("push " + iff);
                emit("jump");

                freshName();
                emitLabel(iff);

                lex.eat("DO");

                while (isStm(lex.tok().type)) {
                    executedIf = true;
                    stm();
                }

                emit("push " + end);
                emit("jump");

                emitLabel(eldo);
                alt();
                emitLabel(end);
                break;


            case "REPEAT":
                lex.next();
                String loop = freshName("loop");
                emitLabel(loop);
                while (isStm(lex.tok().type)) {
                    stm();
                }
                lex.eat("UNTIL");
                exp();
                emit("push " + loop);
                emit("jump_z");
                lex.eat("SEMIC");
                break;

            case "PRINTINT":
                lex.next();
                exp();
                lex.eat("SEMIC");
                emit("push 3");
                emit("sysc");
                break;

            case "RETURN":
                lex.next();
                if (!isfun) {
                    exp();
                    emit("halt");
                    lex.eat("SEMIC");
                } else {
                    rtn = true;
                    exp();

                    emit("get_fp");
                    emit("push 1");
                    emit("add");
                    emit("load");
                    int temp = parametercount + 1;
                    emit("get_fp");
                    emit("load");
                    emit("set_fp");
                    emit("push " + temp);
                    emit("ret_v");
                    lex.eat("SEMIC");
                    break;
                }

            case "PRINTCHAR":
                lex.next();
                exp();
                lex.eat("SEMIC");
                emit("push 1");
                emit("sysc");

                break;

            default:
                throw new ParseException(lex.tok(), "ID, IF, REPEAT, PRINTINT, or PRINTCHAR");
        }

    }
    private boolean isStm(String tokenType) {
        return tokenType.equals("ID") ||
                tokenType.equals("IF") ||
                tokenType.equals("PP") ||
                tokenType.equals("REPEAT") ||
                tokenType.equals("PRINTINT") ||
                tokenType.equals("RETURN") ||
                tokenType.equals("PRINTCHAR");
    }


    private void varListRest(String s) {
        if (lex.tok().type.equals("COMMA")) {
            lex.next();

            if (s.equals("g")) {
                String varName = lex.tok().image;
                lex.eat("ID");
                globalVars.put(varName, 0);
                varListRest("g");
            }

            if (s.equals("ac")) {
                parameters.put(lex.tok().image, parametersdefcount);
                parametersdefcount++;
                lex.next();
                varListRest("ac");
            }

            if (s.equals("l")) {
                String varName = lex.tok().image;
                localCount++;
                //
                int place = localCount;
                localVars.put(varName, place);

                lex.next();
                varListRest("l");

                if (parameters.containsKey(varName)) {
                    throw new ParseException(lex.tok(), "ID");
                }
            }

        }
    }

    //actual parameter list
    private void varList(String s) {
        if (lex.tok().type.equals("ID")) {
            parametersdefcount = 2;
            parameters.put(lex.tok().image, parametersdefcount);
            parametersdefcount++;
            lex.eat("ID");
            varListRest(s);
        }
    }

    private void locals() {
        if (lex.tok().type.equals("LOCALS")) {
            lex.next();
            String varName = lex.tok().image;
            if (parameters.containsKey(varName)) {
                throw new ParseException(lex.tok(), "ID");
            }
            lex.eat("ID");
            localCount++;
            int place = localCount;
            localVars.put(varName, place);
            varListRest("l");
            lex.eat("SEMIC");
        }

    }
    private void functiondef() {

        switch (lex.tok().type) {
            case "DEF":

                isfun = true;
                lex.next();
                String defname = lex.tok().image;

                lex.eat("ID");
                lex.eat("LBR");
                varList("ac");
                lex.eat("RBR");

                lex.eat("LCBR");
                if (!idMap.containsKey(defname)) {
                    id = freshName();
                    idMap.put(defname, id);
                }
                if (idMap.containsKey(defname)) {

                    emitLabel(idMap.get(defname));

                }
                //emitLabel(id);

                emit("get_sp");
                emit("push 1");
                emit("sub");
                emit("set_sp");
                emit("get_fp");
                emit("get_sp");
                emit("store");
                emit("get_sp");
                emit("set_fp");
                emit(" ");


                locals();
                if (localCount > 0) {
                    emit("get_sp");
                    emit("push " + localCount);
                    emit("sub");
                    emit("set_sp");
                }

                while (isStm(lex.tok().type)) {
                    stm();
                }

                if (!rtn) {

                    emit("push 0");

                    emit("get_fp");
                    emit("push 1");
                    emit("add");
                    emit("load");


                    int temp = parametercount;
                    emit("get_fp");
                    emit("load");
                    emit("set_fp");
                    emit("push " + temp);
                    emit("ret_v");


                }
                lex.eat("RCBR");
                break;
        }
        rtn = false;
    }

    private void MAIN() {
        switch (lex.tok().type) {
            case "MAIN":

                lex.next();
                lex.eat("LCBR");
                globals();
                while (isStm(lex.tok().type)) {
                    stm();
                }
                emit("halt");
                lex.eat("RCBR");

                while (lex.tok().type.equals("DEF")) {

                    functiondef();

                    parametercount = 1;
                    localCount = 0;
                    parameters = new HashMap<>();

                }

                break;
            default:
                throw new ParseException(lex.tok(), "MAIN");
        }
    }
    public void Prog() {
        MAIN();

        for (int i = 0; i < endlist.size(); i++) {
            emitLabel(endlist.get(i));
        }
        emit("halt");
        // To be completed
    }

    public static void main(String[] args) throws IOException {
        if (args.length != 3) {
            throw new Error("Usage: java LP23xCompiler <sbnf-file> <source-file> <output-file>");
        }
        String sbnfFilePath = args[0];
        String sourceFilePath = args[1];
        try (PrintStream outFilePath = new PrintStream(new FileOutputStream(args[2]))) {
            LP23xCompiler compiler = new LP23xCompiler(sbnfFilePath, outFilePath);
            compiler.compile(sourceFilePath);
        }
    }
}
