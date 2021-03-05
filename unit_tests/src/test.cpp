// test.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <iostream>
#include <fstream>
#include <map>
#include <functional>
#include <random>
#include <neonumeric/integer.hpp>
#include <neonumeric/real.hpp>

template class neonumeric::xinteger<0, neonumeric::integer_type::Signed, false>;
template class neonumeric::xinteger<100, neonumeric::integer_type::Signed, false>;
template class neonumeric::xinteger<0, neonumeric::integer_type::Unsigned, false>;
template class neonumeric::xinteger<100, neonumeric::integer_type::Unsigned, false>;

template class neonumeric::xinteger<0, neonumeric::integer_type::Signed, true>;
template class neonumeric::xinteger<100, neonumeric::integer_type::Signed, true>;
template class neonumeric::xinteger<0, neonumeric::integer_type::Unsigned, true>;
template class neonumeric::xinteger<100, neonumeric::integer_type::Unsigned, true>;

typedef neonumeric::uinteger<0u, false> unsigned_number;
typedef neonumeric::integer<0u, false> signed_number;
typedef neonumeric::real<0u, false> real_number;

namespace test
{
    bool onlyShowFailures = true;
    bool showNative = false;

    template <typename NumberType>
    bool output(uint32_t inputBase, uint32_t outputBase, const std::string& expression, const std::string& check = std::string{}, bool testArguments = false, bool onlyShowFailures = test::onlyShowFailures)
    {
        std::string x, op, y;
        std::istringstream iss{ expression };
        iss >> x >> op >> y;

        typedef NumberType number;
        auto nx = number::from_string(x, inputBase);
        auto ny = number::from_string(y, inputBase);
        std::map<std::string, std::function<number(const number & x, const number & y)>> f =
        {
            { "+", [](const number& x, const number& y) -> number { return x + y; } },
            { "-", [](const number& x, const number& y) -> number { return x - y; } },
            { "*", [](const number& x, const number& y) -> number { return x * y; } },
            { "/", [](const number& x, const number& y) -> number { return x / y; } },
            { "%", [](const number& x, const number& y) -> number { return x % y; } },
            { "<", [](const number& x, const number& y) -> number { return x < y; } },
            { ">", [](const number& x, const number& y) -> number { return x > y; } },
            { "<=", [](const number& x, const number& y) -> number { return x <= y; } },
            { ">=", [](const number& x, const number& y) -> number { return x >= y; } },
            { "==", [](const number& x, const number& y) -> number { return x == y; } },
            { "!=", [](const number& x, const number& y) -> number { return x != y; } },
            { "pow", [](const number& x, const number& y) -> number { return neonumeric::pow(x, y); } },
        };
        if constexpr (number::IsInteger)
        {
            f["<<"] = [](const number& x, const number& y) -> number { return x << y; };
            f[">>"] = [](const number& x, const number& y) -> number { return x >> y; };
            f["frac"] = [](const number& x, const number& y) -> number { return number{ neonumeric::frac<typename number::real_type>(x, y) }; };
        }
        else
            f["frac"] = [](const number& x, const number& y) -> number { return neonumeric::frac<number>(typename number::signed_integer_type{ x }, typename number::signed_integer_type{ y }); };
        if (f.find(op) == f.end())
            return false;
        if (testArguments)
        {
            if constexpr (number::IsInteger)
            {
                std::cout << (number::IsSigned ? "(signed) " : "(unsigned) ") << "    x: " << nx << std::endl;
                std::cout << (number::IsSigned ? "(signed) " : "(unsigned) ") << "   -x: " << -nx << std::endl;
                std::cout << (number::IsSigned ? "(signed) " : "(unsigned) ") << "-(-x): " << -(-nx) << std::endl;
                std::cout << (number::IsSigned ? "(signed) " : "(unsigned) ") << "    y: " << ny << std::endl;
                std::cout << (number::IsSigned ? "(signed) " : "(unsigned) ") << "   -y: " << -ny << std::endl;
                std::cout << (number::IsSigned ? "(signed) " : "(unsigned) ") << "-(-y): " << -(-ny) << std::endl;
            }
            else
            {
                std::cout << "(real)     x: " << nx << std::endl;
                std::cout << "(real)    -x: " << -nx << std::endl;
                std::cout << "(real) -(-x): " << -(-nx) << std::endl;
                std::cout << "(real)     y: " << ny << std::endl;
                std::cout << "(real)    -y: " << -ny << std::endl;
                std::cout << "(real) -(-y): " << -(-ny) << std::endl;
            }
        }
        auto result = f[op](nx, ny);
        auto resultAsString = number::IsReal ? result.to_string(outputBase, static_cast<uint32_t>(std::cout.precision())) : result.to_string(outputBase);
        std::ostringstream oss;
        oss << std::setprecision(8) << std::fixed;
        if constexpr (number::IsInteger)
            oss << (number::IsSigned ? "(signed) " : "(unsigned) ") << x << " " << op << " " << y << " = " << resultAsString;
        else if (showNative)
            oss << "(real) " << x << " (as double: " << static_cast<double>(nx) << ") " << op << " " << y << " (as double: " << static_cast<double>(ny) << ")" << " = " << resultAsString << " (as double: " << static_cast<double>(result) << ")";
        else
            oss << "(real) " << x << " " << op << " " << y << " = " << resultAsString;
        if (check.empty() || resultAsString == check)
        {
            if (!onlyShowFailures || check.empty())
            {
                std::cout << oss.str() << (check.empty() ? "" : " [PASS]") << std::endl;
            }
        }
        else
        {
            oss << " [FAIL] (correct: " << check << ")";
            std::cerr << oss.str() << std::endl;
            return false;
        }

        return true;
    }

    template <typename NumberType>
    void output_test(uint32_t& testFailureCount, uint32_t& testPassCount, uint32_t inputBase, uint32_t outputBase, const std::string& expression, const std::string& check = std::string{}, bool testArguments = false, bool onlyShowFailures = test::onlyShowFailures)
    {
        if (!output<NumberType>(inputBase, outputBase, expression, check, testArguments, onlyShowFailures))
            ++testFailureCount;
        else
            ++testPassCount;
    }

    template <typename NumberType>
    void test_equality(uint32_t& testFailureCount, uint32_t& testPassCount, NumberType const& lhs, NumberType const& rhs)
    {
        if (lhs != rhs)
            ++testFailureCount;
        else
            ++testPassCount;
    }

    void run_tests()
    {
        uint32_t failedTests = 0u;
        uint32_t passedTests = 0u;

        double constexpr LOW_PRECISION_EPSILON = 0.001;
        double constexpr HIGH_PRECISION_EPSILON = 0.0000001;

        auto loop_test = [&](double x, double y, double epsilon)
        {
            if (std::abs(x) < epsilon)
                x = 0.0;
            if (std::abs(y) < epsilon)
                y = 0.0;
            real_number const rx = x;
            real_number const ry = y;
            auto const s = rx + ry;
            auto const d = rx - ry;
            auto const p = rx * ry;
            auto const q = rx / ry;
            bool sPass = std::abs(static_cast<double>(s) - (x + y)) < epsilon && (static_cast<double>(s) > 0.0) == ((x + y) > 0.0);
            bool dPass = std::abs(static_cast<double>(d) - (x - y)) < epsilon && (static_cast<double>(d) > 0.0) == ((x - y) > 0.0);
            bool pPass = std::abs(static_cast<double>(p) - (x * y)) < epsilon && (static_cast<double>(p) > 0.0) == ((x * y) > 0.0);
            bool qPass = std::abs(static_cast<double>(q) - (x / y)) < epsilon && (static_cast<double>(q) > 0.0) == ((x / y) > 0.0);
            if (!qPass)
            {
                if (static_cast<double>(q) == x / y) // for special values: inf, NaN
                    qPass = true;
                else if (std::isnan(static_cast<double>(q)) && std::isnan(x / y))
                    qPass = true;
            }
            if (!sPass)
                ++failedTests;
            else
                ++passedTests;
            if (!dPass)
                ++failedTests;
            else
                ++passedTests;
            if (!pPass)
                ++failedTests;
            else
                ++passedTests;
            if (!qPass)
                ++failedTests;
            else
                ++passedTests;
            if (!sPass || !onlyShowFailures)
                (sPass ? std::cout : std::cerr) << rx << " (" << x << ") + " << ry << " (" << y << ") = " << s << " (" << static_cast<double>(s) << ") " << (sPass ? "[PASS] " : "[FAIL] ");
            if (!sPass)
                std::cerr << "(correct: " << x + y << ")" << std::endl;
            else if (!onlyShowFailures)
                std::cout << std::endl;
            if (!dPass || !onlyShowFailures)
                (dPass ? std::cout : std::cerr) << rx << " (" << x << ") - " << ry << " (" << y << ") = " << d << " (" << static_cast<double>(d) << ") " << (dPass ? "[PASS] " : "[FAIL] ");
            if (!dPass)
                std::cerr << "(correct: " << x - y << ")" << std::endl;
            else if (!onlyShowFailures)
                std::cout << std::endl;
            if (!pPass || !onlyShowFailures)
                (pPass ? std::cout : std::cerr) << rx << " (" << x << ") * " << ry << " (" << y << ") = " << p << " (" << static_cast<double>(p) << ") " << (pPass ? "[PASS] " : "[FAIL] ");
            if (!pPass)
                std::cerr << "(correct: " << x * y << ")" << std::endl;
            else if (!onlyShowFailures)
                std::cout << std::endl;
            if (!qPass || !onlyShowFailures)
                (qPass ? std::cout : std::cerr) << rx << " (" << x << ") / " << ry << " (" << y << ") = " << q << " (" << static_cast<double>(q) << ") " << (qPass ? "[PASS] " : "[FAIL] ");
            if (!qPass)
                std::cerr << "(correct: " << x / y << ")" << std::endl;
            else if (!onlyShowFailures)
                std::cout << std::endl;
        };

        std::mt19937_64 prng;
        std::uniform_real_distribution<> dis(-1000000.0, 1000000.0);

        for (int i = 0; i < 100; ++i)
        {
            loop_test(dis(prng), dis(prng), LOW_PRECISION_EPSILON);
        }

        for (double x = -2.1; x <= 2.1; x += 0.1)
            for (double y = -2.1; y <= 2.1; y += 0.1)
                loop_test(x, y, HIGH_PRECISION_EPSILON);

        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "-0 + -0", "0");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "-0 + 0", "0");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "0 + -0", "0");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "0 + 0", "0");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "-0 - -0", "0");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "-0 - 0", "0");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "0 - -0", "0");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "0 - 0", "0");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "-1 + -2", "-3");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "-1 + 2", "1");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "1 + -2", "-1");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "1 + 2", "3");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "-1 - -2", "1");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "-1 - 2", "-3");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "1 - -2", "3");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "1 - 2", "-1");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "-2 + -1", "-3");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "-2 + 1", "-1");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "2 + -1", "1");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "2 + 1", "3");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "-2 - -1", "-1");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "-2 - 1", "-3");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "2 - -1", "3");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "2 - 1", "1");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "-1 + -1", "-2");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "-1 + 1", "0");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "1 + -1", "0");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "1 + 1", "2");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "-1 - -1", "0");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "-1 - 1", "-2");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "1 - -1", "2");
        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "1 - 1", "0");

        output_test<signed_number>(failedTests, passedTests, 10u, 10u, "10000000000000000000000000000000000000000000 * 1000000000000000000000000000000000000000000000", 
            "10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");

        output_test<signed_number>(failedTests, passedTests, 10u, 10u,
            "850359753498057983247598324759832475983247598327495832498573429857938425983425798324 * 5832409834290583420985903248565783246589723458632489756837245873245687324658732458734265873",
            "4959646588986989727635694821764896478591749883193312239661479260735998239885755708141504579483055817135561248151273356715520440747723401326376430484743812701546827729093796852");

        output_test<signed_number>(failedTests, passedTests, 10u, 10u,
            "6657815786415797148305678943754395615014578645897623487568234756837241056342987658732465983725698372456872365872345687324659872346589732465872346593827465983274658732456837246583724568372465873246587324658732465302948563028947560398245689732465873246587342658732468573485324658736873245682356367787843978432978983293838383563485697329587568756875837425679234568234756782333232332325768456238947562346573246587324658723652389475698723456872356892736587236578236598723456987342658793246587234867523468532467563248756382475687234956827365732495723496532459836325846086512360465813468564358115656301845613475678324678236874236874871232314218 * 674515378465123876412376548761235476237564763254721376098435769834570698345769083547690837459867345986798453679805743098693458765624386548793246598734658723465723465873426587234679562374568723465983674589769834576984537609853470693457698453768945376893457698457986074598067983456783456784537683457698345763457698345769834576435076034598679834506798609784530786345786798403512396848123796487123648791236874123687468213746213468791236497823648712367423646817234687123641236487367412638746128937698043567983456793485679845376890345076834576980345768934576983457690834576890453769083457698453768943567908435769803457638945673495865234785643278563248795678234568723456873426587234687523648756873245623874562348568237", 
            "4490799134925327765677957905833038412114477510166403668022639438217895407855305141601995223571127404617702509303193220714486492440613757708786140885319090201211179511722911060438917329699942422240962920395685386559487245157866939625156291642135585959574261962785733232765928950585507597066423121082329677787937815978566732398237635698256027869585884754775276608374309123395979990911210405826483325439334741139353868452114853123160289637115393837760805390947490119606303089962433726362527017080488326862725331737347766662328217719511629664878698864960578227954476386396675062204650506213572656563823206286857108583722668931903945161299368357096028756673232218896173139047845351929071048143480244733492080686485908344818282893016494926901202956322459135832499809805141309494351228377486009714109885785227187048630478410885917875751528556859229369904419384605805961017843768238210238830545215728910647688932901863039584427838230587725890858430230445523100439026591661968509426760217081190400191644527664331918751027643716120915706242080155729012515013356928791045860045553600788761101956361636670571214191999271058153094619959476668828691360411175291132625699644045599110786720079161158275269618036193174622147960005773610987860794157003731122296555368905095306686741879158077698043984969668557943847645936943913777949172089097302037256172300398293666");

        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "-0 + -0", "0");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "-0 + 0", "0");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "0 + -0", "0");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "0 + 0", "0");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "-0 - -0", "0");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "-0 - 0", "0");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "0 - -0", "0");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "0 - 0", "0");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "-1 + -2", "36893488147419103229");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "-1 + 2", "18446744073709551617");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "1 + -2", "18446744073709551615");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "1 + 2", "3");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "-1 - -2", "1");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "-1 - 2", "18446744073709551613");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "1 - -2", "3");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "1 - 2", "18446744073709551615");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "-2 + -1", "36893488147419103229");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "-2 + 1", "18446744073709551615");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "2 + -1", "18446744073709551617");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "2 + 1", "3");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "-2 - -1", "18446744073709551615");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "-2 - 1", "18446744073709551613");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "2 - -1", "3");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "2 - 1", "1");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "-1 + -1", "36893488147419103230");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "-1 + 1", "18446744073709551616");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "1 + -1", "18446744073709551616");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "1 + 1", "2");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "-1 - -1", "0");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "-1 - 1", "18446744073709551614");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "1 - -1", "2");
        output_test<unsigned_number>(failedTests, passedTests, 10u, 10u, "1 - 1", "0");

        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "-0 + -0", "0");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "-0 + 0", "0");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "0 + -0", "0");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "0 + 0", "0");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "-0 - -0", "0");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "-0 - 0", "0");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "0 - -0", "0");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "0 - 0", "0");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "-1 + -2", "-3");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "-1 + 2", "1");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "1 + -2", "-1");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "1 + 2", "3");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "-1 - -2", "1");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "-1 - 2", "-3");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "1 - -2", "3");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "1 - 2", "-1");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "-2 + -1", "-3");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "-2 + 1", "-1");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "2 + -1", "1");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "2 + 1", "3");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "-2 - -1", "-1");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "-2 - 1", "-3");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "2 - -1", "3");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "2 - 1", "1");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "-1 + -1", "-2");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "-1 + 1", "0");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "1 + -1", "0");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "1 + 1", "2");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "-1 - -1", "0");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "-1 - 1", "-2");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "1 - -1", "2");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "1 - 1", "0");

        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "-0 + -0", "0");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "-0 + 0", "0");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "0 + -0", "0");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "0 + 0", "0");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "-0 - -0", "0");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "-0 - 0", "0");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "0 - -0", "0");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "0 - 0", "0");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "-1 + -2", "1FFFFFFFFFFFFFFFD");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "-1 + 2", "10000000000000001");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "1 + -2", "FFFFFFFFFFFFFFFF");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "1 + 2", "3");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "-1 - -2", "1");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "-1 - 2", "FFFFFFFFFFFFFFFD");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "1 - -2", "3");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "1 - 2", "FFFFFFFFFFFFFFFF");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "-2 + -1", "1FFFFFFFFFFFFFFFD");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "-2 + 1", "FFFFFFFFFFFFFFFF");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "2 + -1", "10000000000000001");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "2 + 1", "3");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "-2 - -1", "FFFFFFFFFFFFFFFF");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "-2 - 1", "FFFFFFFFFFFFFFFD");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "2 - -1", "3");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "2 - 1", "1");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "-1 + -1", "1FFFFFFFFFFFFFFFE");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "-1 + 1", "10000000000000000");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "1 + -1", "10000000000000000");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "1 + 1", "2");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "-1 - -1", "0");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "-1 - 1", "FFFFFFFFFFFFFFFE");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "1 - -1", "2");
        output_test<unsigned_number>(failedTests, passedTests, 16u, 16u, "1 - 1", "0");

        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "1234FFFFFFFFFFFFFFFF - 12340000000000000000", "FFFFFFFFFFFFFFFF");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u,
            "100000000000000000000000000000000000000000000000000000000000000000000000 * 100000000000000000000000000000000000000000000000000000000000000000000000",
            "10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
        output_test<signed_number>(failedTests, passedTests, 16u, 16u, "1AD0326BEA9359254FAF1EBA1368F19BF180354A7F7F99B35721AC3241AF9E28D4650C36F879223BB2AA2D384497F4A1C62790171189887A10000"
            " * " "1AD0326BEA9359254FAF1EBA1368F19BF180354A7F7F99B35721AC3241AF9E28D4650C36F879223BB2AA2D384497F4A1C62790171189887A10000",
            "2CEF38FE4F172ABEDC1DAE87445D8DEDC89521CB1EE32B4576F8DA02156F04B209E4"
            "B7219B04469700D9D03E2034BDA2490E0015B5EF17DFAF0928856D6ADF69B6B7DB00"
            "A05F62251C99EDEF1DBBC229E3F2BDA49E0B8194534683D5EE7B666E4DC0F39FAE74"
            "4DB676445F271870B334100000000");

        if (failedTests == 0u)
            std::cout << "All " << passedTests << " tests pass! \\o/" << std::endl;
        else
            std::cerr << failedTests << " tests FAILED! (" << passedTests << " tests passed.)" <<std::endl;
    }

    enum class mode
    {
        Any,
        Integer,
        Real
    };

    enum class integer_mode
    {
        Signed,
        Unsigned
    };
}

int main()
{
    std::cout << "neonumeric test program\n";

    std::cout << std::setprecision(11) << std::fixed;
    std::cerr << std::setprecision(11) << std::fixed;

    test::mode mode = test::mode::Any;
    test::integer_mode integerMode = test::integer_mode::Signed;

    uint32_t ibase = 10u;
    uint32_t obase = 10u;

    std::function<void(std::istream & stream)> process = [&](std::istream& stream)
    {
        while (stream)
        {
            std::cout << "<x> <op> <y> (" << 
                (mode == test::mode::Any ? "any, " : mode == test::mode::Integer ? "integer, " : "real, ") << 
                (mode == test::mode::Any || mode == test::mode::Integer ? integerMode == test::integer_mode::Signed ? "signed, " : "unsigned, " : "") <<
                (ibase == 10u ? "dec, " : ibase == 16u ? "hex, " : ibase == 8u ? "oct, " : ibase == 2u ? "bin, " : "n, ") <<
                "precision: " << std::cout.precision() << "): ";
            std::string::size_type f;
            std::string line;
            std::getline(stream, line);
            if (line == "test")
                test::run_tests();
            else if (line == "all")
                test::onlyShowFailures = false;
            else if (line == "only")
                test::onlyShowFailures = true;
            else if (line == "dec")
            {
                ibase = 10u;
                obase = 10u;
            }
            else if (line == "hex")
            {
                ibase = 16u;
                obase = 16u;
            }
            else if (line == "oct")
            {
                ibase = 8u;
                obase = 8u;
            }
            else if (line == "bin")
            {
                ibase = 2u;
                obase = 2u;
            }
            else if (line == "idec")
                ibase = 10u;
            else if (line == "odec")
                obase = 10u;
            else if (line == "ihex")
                ibase = 16u;
            else if (line == "ohex")
                obase = 16u;
            else if (line == "ioct")
                ibase = 8u;
            else if (line == "ooct")
                obase = 8u;
            else if (line == "ibin")
                ibase = 2u;
            else if (line == "obin")
                obase = 2u;
            else if (line == "real")
                mode = test::mode::Real;
            else if (line == "integer")
                mode = test::mode::Integer;
            else if (line == "any")
                mode = test::mode::Any;
            else if (line == "signed")
                integerMode = test::integer_mode::Signed;
            else if (line == "unsigned")
                integerMode = test::integer_mode::Unsigned;
            else if (line == "any")
                mode = test::mode::Any;
            else if (line.find("sn") == 0)
                test::showNative = true;
            else if (line.find("pr") == 0)
            {
                std::istringstream iss{ line };
                std::string ignore;
                uint32_t precision;
                if (iss >> ignore >> precision)
                {
                    std::cout << std::setprecision(precision) << std::fixed;
                    std::cerr << std::setprecision(precision) << std::fixed;
                }
            }
            else if ((f = line.find("load")) == 0)
            {
                std::ifstream file{ line.substr(5) };
                process(file);
            }
            else if (mode == test::mode::Integer || (mode == test::mode::Any && line.find('.') == std::string::npos))
            {
                if (integerMode == test::integer_mode::Signed)
                    test::output<signed_number>(ibase, obase, line);
                else
                    test::output<unsigned_number>(ibase, obase, line);
            }
            else
                test::output<real_number>(ibase, obase, line);
        }
    };

    process(std::cin);
}

