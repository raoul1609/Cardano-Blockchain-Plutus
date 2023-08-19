/* ici c'est le offchain-code du script deployé avec des parametres : le pubkeyhash et le deadline 
    - la difficulte ici est de passer la date au script de minting pour qu'il verifie si le deadline est passé ou non! */

import {
    Lucid,
    Blockfrost,
    Address,
    MintingPolicy,
    PolicyId,
    Unit,
    fromText,
    Data,
    getAddressDetails,
    applyParamsToScript,
AddressDetails
} from "https://deno.land/x/lucid@0.9.1/mod.ts"

function readAmount () : bigint {
    const input = prompt ("entrer la valeur du token");
    return input? BigInt (Number.parseInt (input)) : 100000n 
}

// const test = readAmount ();
// console.log(test)

// const homework1Week05Plicy : MintingPolicy = {
//     type : "PlutusV2",
//     script : "590a71590a6e010000333232323322323232323232323232323322323322323232323233322232323222223232533532323253353235001222222222222533533355301712001321233001225335002210031001002502525335333573466e3c05c0040cc0c84d409c00454098010840cc40c540044d4d5400488888888888801488c8c8d400488d40148c8c8c8c894ccd54ccd4020854ccd401c854ccd402884c015261300449854ccd402084c01526130044984074406c54ccd402484c015261300449854ccd401c84c0152613004498407054ccd401884068406c406454ccd4018854ccd402484c019261300549854ccd401c84c01926130054984070406854ccd402084c019261300549854ccd401884c0192613005498406c54cd401c54cd400440c040c440c040c440c094ccd4014854ccd4020854ccd401c84ccd4068030008004585858406854ccd401c854ccd401884ccd406402c0080045858584064406094ccd4010854ccd401c854ccd401884ccd406402c008004585858406454ccd4018854ccd401484ccd40600280080045858584060405c94ccd400c854ccd4018854ccd401484ccd4060028008004585858406054ccd4014854ccd401084ccd405c024008004585858405c405894ccd4008854ccd4014854ccd401084ccd405c024008004585858405c54ccd4010854ccd400c84ccd405802000800458585840584054cd4038d409002c0a048cccccccc00488ccd5cd19b8700200102a029225335333573466e1c0080040a80a4405054cd4ccd5cd19b8900200102a0291012101322333573466e200080040a80a488ccd5cd19b8900200102a02922333573466e240080040a40a888ccd5cd19b8800200102902a225335333573466e240080040a80a440044008894cd4ccd5cd19b8900200102a029100210011024135001220023333573466e1cd55cea801a4000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4074078d5d0a80619a80e80f1aba1500b33501d01f35742a014666aa042eb94080d5d0a804999aa810bae502035742a01066a03a0546ae85401cccd540840add69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40d5d69aba150023037357426ae8940088c98c80e4cd5ce01e01d81b89aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa0049000119a81019a81abad35742a004606e6ae84d5d1280111931901c99ab9c03c03b037135573ca00226ea8004d5d09aba2500223263203533573807006e06626aae7940044dd50009aba1500533501d75c6ae854010ccd5408409c8004d5d0a801999aa810bae200135742a00460526ae84d5d1280111931901899ab9c03403302f135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00660326ae84d5d1280191931901199ab9c0260250213333573466e1cd55ce9baa0044800080908c98c8088cd5ce012812010081189931901099ab9c49010350543500023135573ca00226ea8004c8004d5407888448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000448848cc00400c00848d40048888888801c488800c4888008488800448c88c008dd6000990009aa80c911999aab9f0012500a233500930043574200460066ae880080648c8c8cccd5cd19b8735573aa004900011991091980080180118071aba150023005357426ae8940088c98c805ccd5ce00d00c80a89aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180b9aba1500233500f016357426ae8940088c98c8070cd5ce00f80f00d09aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6403c66ae7008408007006c0684d55cea80089baa00135742a00466a016eb8d5d09aba2500223263201833573803603402c26ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355016223233335573e0044a010466a00e66442466002006004600c6aae754008c014d55cf280118021aba200301713574200222440042442446600200800624464646666ae68cdc3a800a400046a00e600a6ae84d55cf280191999ab9a3370ea00490011280391931900999ab9c016015011010135573aa00226ea800448488c00800c44880048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900899ab9c01401300f00e00d00c135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900699ab9c01000f00b135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c802ccd5ce00700680489baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c8050cd5ce00b80b00900880800780700680609aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6401a66ae7004003c02c0284d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200a33573801a01801000e26aae7540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea004900111a80398031aba135573ca00846666ae68cdc3a801a400042444004464c6401666ae7003803402402001c4d55cea80089baa00112122230030042323333573466e1d40052002200623333573466e1d40092000200623263200633573801201000800626aae74dd5000a4c2440042440022400292010350543100112323001001223300330020020014891c91dbe3d70568b1ada2b0f059a8394dffb8fa632214c42c66f87fde9500482ba9786f2c7070bf0b81"
// };

const lucid = await Lucid.new(
    new Blockfrost(
        "https://cardano-preprod.blockfrost.io/api/v0",
        "preprodcm3OA5nBkHcJy8NSKoHjHGhxgTDkNPzw"
    ),
    "Preprod"
);

lucid.selectWalletFromSeed("walk dove anchor away giant tilt subway finger donkey party reflect fresh improve say cruel corn dynamic ring barely wink require grab truly rule");
const addr: Address = await lucid.wallet.address();
console.log("mon addresse nami : " + addr);

const addressDetails : AddressDetails = getAddressDetails (addr);
const someHash: string = addressDetails.paymentCredential?.hash || "";

// ici c'est le deadline 
const someDate: Date = new Date("2023-08-19T16:00:00Z")
const someDatePosix = BigInt(someDate.getTime());



const homework1Week05Policy: MintingPolicy = {
    type: "PlutusV2",
    script: "590ac2590abf0100003332323233223232323232323232323233223233223232323232333222323232222232325335323232533553353235001222222222222533533355301712001321233001225335002210031001002502525335333573466e3c05c0040cc0c84d409c00454098010840cc40c5400440944cd5ce248121746865206f776e7765722068617665206e6f74207369676e656420746865207478000241533535355001222222222222005223232350012235005232323232253335533350082153335007215333500a2130054984c011261533350082130054984c01126101d101b1533350092130054984c011261533350072130054984c01126101c1533350062101a101b101915333500621533350092130064984c015261533350072130064984c01526101c101a1533350082130064984c015261533350062130064984c01526101b1533500715335001103010311030103110302533350052153335008215333500721333501a00c002001161616101a153335007215333500621333501900b002001161616101910182533350042153335007215333500621333501900b0020011616161019153335006215333500521333501800a002001161616101810172533350032153335006215333500521333501800a002001161616101815333500521533350042133350170090020011616161017101625333500221533350052153335004213335017009002001161616101715333500421533350032133350160080020011616161016101533500e3502400b028123333333300122333573466e1c0080040a80a4894cd4ccd5cd19b8700200102a029101415335333573466e240080040a80a44048404c88ccd5cd19b8800200102a02922333573466e240080040a80a488ccd5cd19b8900200102902a22333573466e200080040a40a8894cd4ccd5cd19b8900200102a02910011002225335333573466e240080040a80a44008400440944cd5ce24811b74686520646561646c696e6520686173206e6f7420706173736564000241024135001220023333573466e1cd55cea801a4000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4074078d5d0a80619a80e80f1aba1500b33501d01f35742a014666aa042eb94080d5d0a804999aa810bae502035742a01066a03a0546ae85401cccd540840add69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40d5d69aba150023037357426ae8940088c98c80e4cd5ce01e01d81b89aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa0049000119a81019a81abad35742a004606e6ae84d5d1280111931901c99ab9c03c03b037135573ca00226ea8004d5d09aba2500223263203533573807006e06626aae7940044dd50009aba1500533501d75c6ae854010ccd5408409c8004d5d0a801999aa810bae200135742a00460526ae84d5d1280111931901899ab9c03403302f135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00660326ae84d5d1280191931901199ab9c0260250213333573466e1cd55ce9baa0044800080908c98c8088cd5ce012812010081189931901099ab9c4910350543500023135573ca00226ea8004c8004d5407888448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000448848cc00400c00848d40048888888801c488800c4888008488800448c88c008dd6000990009aa80c911999aab9f0012500a233500930043574200460066ae880080648c8c8cccd5cd19b8735573aa004900011991091980080180118071aba150023005357426ae8940088c98c805ccd5ce00d00c80a89aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180b9aba1500233500f016357426ae8940088c98c8070cd5ce00f80f00d09aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6403c66ae7008408007006c0684d55cea80089baa00135742a00466a016eb8d5d09aba2500223263201833573803603402c26ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355016223233335573e0044a010466a00e66442466002006004600c6aae754008c014d55cf280118021aba200301713574200222440042442446600200800624464646666ae68cdc3a800a400046a00e600a6ae84d55cf280191999ab9a3370ea00490011280391931900999ab9c016015011010135573aa00226ea800448488c00800c44880048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900899ab9c01401300f00e00d00c135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900699ab9c01000f00b135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c802ccd5ce00700680489baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c8050cd5ce00b80b00900880800780700680609aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6401a66ae7004003c02c0284d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200a33573801a01801000e26aae7540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea004900111a80398031aba135573ca00846666ae68cdc3a801a400042444004464c6401666ae7003803402402001c4d55cea80089baa00112122230030042323333573466e1d40052002200623333573466e1d40092000200623263200633573801201000800626aae74dd5000a4c2440042440022400292010350543100112323001001223300330020020014891c2bf3b747ec2983fe13b1f6844501f8447279360e38467e14b59f0eeb0048332da1b38301"
};

// const Params = Data.Tuple([Data.String, Data.BigInt]);
// type Params = Data.Static<typeof Params>;
// const homework1Week05Policy: MintingPolicy = {
//     type: "PlutusV2",
//     script: applyParamsToScript<Params>(
//         "590a71590a6e010000333232323322323232323232323232323322323322323232323233322232323222223232533532323253353235001222222222222533533355301712001321233001225335002210031001002502525335333573466e3c05c0040cc0c84d409c00454098010840cc40c540044d4d5400488888888888801488c8c8d400488d40148c8c8c8c894ccd54ccd4020854ccd401c854ccd402884c015261300449854ccd402084c01526130044984074406c54ccd402484c015261300449854ccd401c84c0152613004498407054ccd401884068406c406454ccd4018854ccd402484c019261300549854ccd401c84c01926130054984070406854ccd402084c019261300549854ccd401884c0192613005498406c54cd401c54cd400440c040c440c040c440c094ccd4014854ccd4020854ccd401c84ccd4068030008004585858406854ccd401c854ccd401884ccd406402c0080045858584064406094ccd4010854ccd401c854ccd401884ccd406402c008004585858406454ccd4018854ccd401484ccd40600280080045858584060405c94ccd400c854ccd4018854ccd401484ccd4060028008004585858406054ccd4014854ccd401084ccd405c024008004585858405c405894ccd4008854ccd4014854ccd401084ccd405c024008004585858405c54ccd4010854ccd400c84ccd405802000800458585840584054cd4038d409002c0a048cccccccc00488ccd5cd19b8700200102a029225335333573466e1c0080040a80a4405054cd4ccd5cd19b8900200102a0291012101322333573466e200080040a80a488ccd5cd19b8900200102a02922333573466e240080040a40a888ccd5cd19b8800200102902a225335333573466e240080040a80a440044008894cd4ccd5cd19b8900200102a029100210011024135001220023333573466e1cd55cea801a4000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4074078d5d0a80619a80e80f1aba1500b33501d01f35742a014666aa042eb94080d5d0a804999aa810bae502035742a01066a03a0546ae85401cccd540840add69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40d5d69aba150023037357426ae8940088c98c80e4cd5ce01e01d81b89aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa0049000119a81019a81abad35742a004606e6ae84d5d1280111931901c99ab9c03c03b037135573ca00226ea8004d5d09aba2500223263203533573807006e06626aae7940044dd50009aba1500533501d75c6ae854010ccd5408409c8004d5d0a801999aa810bae200135742a00460526ae84d5d1280111931901899ab9c03403302f135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00660326ae84d5d1280191931901199ab9c0260250213333573466e1cd55ce9baa0044800080908c98c8088cd5ce012812010081189931901099ab9c49010350543500023135573ca00226ea8004c8004d5407888448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000448848cc00400c00848d40048888888801c488800c4888008488800448c88c008dd6000990009aa80c911999aab9f0012500a233500930043574200460066ae880080648c8c8cccd5cd19b8735573aa004900011991091980080180118071aba150023005357426ae8940088c98c805ccd5ce00d00c80a89aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180b9aba1500233500f016357426ae8940088c98c8070cd5ce00f80f00d09aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6403c66ae7008408007006c0684d55cea80089baa00135742a00466a016eb8d5d09aba2500223263201833573803603402c26ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355016223233335573e0044a010466a00e66442466002006004600c6aae754008c014d55cf280118021aba200301713574200222440042442446600200800624464646666ae68cdc3a800a400046a00e600a6ae84d55cf280191999ab9a3370ea00490011280391931900999ab9c016015011010135573aa00226ea800448488c00800c44880048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900899ab9c01401300f00e00d00c135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900699ab9c01000f00b135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c802ccd5ce00700680489baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c8050cd5ce00b80b00900880800780700680609aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6401a66ae7004003c02c0284d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200a33573801a01801000e26aae7540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea004900111a80398031aba135573ca00846666ae68cdc3a801a400042444004464c6401666ae7003803402402001c4d55cea80089baa00112122230030042323333573466e1d40052002200623333573466e1d40092000200623263200633573801201000800626aae74dd5000a4c2440042440022400292010350543100112323001001223300330020020014891c91dbe3d70568b1ada2b0f059a8394dffb8fa632214c42c66f87fde9500482ba9786f2c7070bf0b81",
//         [someHash, somePosixTime],
//         Params)
// };

// const policyId: PolicyId = lucid.utils.mintingPolicyToId(homework1Week05Policy);
// console.log("minting policy: " + policyId);

const policyId: PolicyId = lucid.utils.mintingPolicyToId(homework1Week05Policy);
console.log("minting policy: " + policyId);

const unit: Unit = policyId + fromText("homework1Week05Policy");

const amount: bigint = readAmount();

const tx = await lucid
    .newTx()
    .mintAssets({[unit]: amount}, Data.void())
    .attachMintingPolicy(homework1Week05Policy)
    .addSignerKey (someHash)
    .complete();

const signedTx = await tx.sign().complete();
const txHash = await signedTx.submit();
console.log("tid: " + txHash);
