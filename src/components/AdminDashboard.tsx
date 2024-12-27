import React, { useState, useEffect } from 'react';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { LineChart, XAxis, YAxis, Line, Tooltip } from 'recharts';
import { Shield, AlertTriangle, Settings, Users, Activity } from 'lucide-react';

const AdminDashboard = () => {
    const [selectedTab, setSelectedTab] = useState('health');
    const [contractHealth, setContractHealth] = useState({
        status: 'healthy',
        totalValue: 0n,
        activeGames: 0,
        lastUpdate: new Date()
    });

    const [emergencyState, setEmergencyState] = useState({
        paused: false,
        recoveryMode: false
    });

    return (
        <div className="w-full max-w-7xl mx-auto p-4">
            <Card className="mb-6">
                <CardHeader>
                    <CardTitle className="flex items-center">
                        <Shield className="mr-2" />
                        Admin Dashboard
                    </CardTitle>
                </CardHeader>
                <CardContent>
                    <Tabs value={selectedTab} onValueChange={setSelectedTab}>
                        <TabsList>
                            <TabsTrigger value="health">Contract Health</TabsTrigger>
                            <TabsTrigger value="parameters">Game Parameters</TabsTrigger>
                            <TabsTrigger value="emergency">Emergency Controls</TabsTrigger>
                            <TabsTrigger value="analytics">Analytics</TabsTrigger>
                            <TabsTrigger value="dealers">Dealer Management</TabsTrigger>
                        </TabsList>

                        <TabsContent value="health">
                            <ContractHealthPanel health={contractHealth} />
                        </TabsContent>

                        <TabsContent value="parameters">
                            <GameParametersPanel />
                        </TabsContent>

                        <TabsContent value="emergency">
                            <EmergencyControlsPanel 
                                state={emergencyState}
                                onPause={() => setEmergencyState(prev => ({ ...prev, paused: !prev.paused }))}
                                onRecovery={() => setEmergencyState(prev => ({ ...prev, recoveryMode: !prev.recoveryMode }))}
                            />
                        </TabsContent>

                        <TabsContent value="analytics">
                            <AnalyticsPanel />
                        </TabsContent>

                        <TabsContent value="dealers">
                            <DealerManagementPanel />
                        </TabsContent>
                    </Tabs>
                </CardContent>
            </Card>
        </div>
    );
};

const ContractHealthPanel = ({ health }) => (
    <div className="space-y-4">
        <div className="grid grid-cols-3 gap-4">
            <Card>
                <CardContent className="pt-6">
                    <div className="flex justify-between items-center">
                        <h3 className="font-semibold">Contract Status</h3>
                        <div className={`h-3 w-3 rounded-full ${
                            health.status === 'healthy' ? 'bg-green-500' : 'bg-red-500'
                        }`} />
                    </div>
                    <p className="text-2xl mt-2 capitalize">{health.status}</p>
                </CardContent>
            </Card>

            <Card>
                <CardContent className="pt-6">
                    <h3 className="font-semibold">Total Value Locked</h3>
                    <p className="text-2xl mt-2">{Number(health.totalValue) / 1_000_000} ADA</p>
                </CardContent>
            </Card>

            <Card>
                <CardContent className="pt-6">
                    <h3 className="font-semibold">Active Games</h3>
                    <p className="text-2xl mt-2">{health.activeGames}</p>
                </CardContent>
            </Card>
        </div>

        <div className="h-[300px]">
            <LineChart data={[]} width={800} height={300}>
                <XAxis dataKey="time" />
                <YAxis />
                <Tooltip />
                <Line type="monotone" dataKey="value" stroke="#8884d8" />
            </LineChart>
        </div>
    </div>
);

const GameParametersPanel = () => (
    <div className="space-y-4">
        <Card>
            <CardContent className="pt-6">
                <h3 className="font-semibold mb-4">Game Configuration</h3>
                <div className="space-y-4">
                    <div>
                        <label className="block text-sm mb-1">Minimum Bet (ADA)</label>
                        <Input type="number" min="1" />
                    </div>
                    <div>
                        <label className="block text-sm mb-1">Maximum Bet (ADA)</label>
                        <Input type="number" min="1" />
                    </div>
                    <div>
                        <label className="block text-sm mb-1">Game Timeout (minutes)</label>
                        <Input type="number" min="1" />
                    </div>
                    <div>
                        <label className="block text-sm mb-1">House Fee (%)</label>
                        <Input type="number" min="0" max="100" step="0.1" />
                    </div>
                    <Button className="w-full">Update Parameters</Button>
                </div>
            </CardContent>
        </Card>
    </div>
);

const EmergencyControlsPanel = ({ state, onPause, onRecovery }) => (
    <div className="space-y-4">
        <Alert variant={state.paused ? "destructive" : "default"}>
            <AlertTriangle className="h-4 w-4" />
            <AlertDescription>
                Emergency controls should only be used in critical situations.
            </AlertDescription>
        </Alert>

        <Card>
            <CardContent className="pt-6">
                <div className="space-y-4">
                    <Button 
                        variant={state.paused ? "destructive" : "default"}
                        className="w-full"
                        onClick={onPause}
                    >
                        {state.paused ? "Resume Contract" : "Pause Contract"}
                    </Button>
                    <Button 
                        variant="destructive" 
                        className="w-full"
                        onClick={onRecovery}
                    >
                        {state.recoveryMode ? "Exit Recovery Mode" : "Enter Recovery Mode"}
                    </Button>
                </div>
            </CardContent>
        </Card>
    </div>
);

const AnalyticsPanel = () => (
    <div className="space-y-4">
        <div className="grid grid-cols-2 gap-4">
            <Card>
                <CardContent className="pt-6">
                    <h3 className="font-semibold">Transaction Success Rate</h3>
                    <p className="text-2xl mt-2">98.5%</p>
                </CardContent>
            </Card>

            <Card>
                <CardContent className="pt-6">
                    <h3 className="font-semibold">Average Game Duration</h3>
                    <p className="text-2xl mt-2">12.5 minutes</p>
                </CardContent>
            </Card>
        </div>

        <div className="h-[300px]">
            <LineChart data={[]} width={800} height={300}>
                <XAxis dataKey="time" />
                <YAxis />
                <Tooltip />
                <Line type="monotone" dataKey="players" stroke="#82ca9d" />
            </LineChart>
        </div>
    </div>
);

const DealerManagementPanel = () => (
    <div className="space-y-4">
        <Card>
            <CardContent className="pt-6">
                <h3 className="font-semibold mb-4">Authorized Dealers</h3>
                <div className="space-y-4">
                    <div>
                        <label className="block text-sm mb-1">Add New Dealer</label>
                        <div className="flex gap-2">
                            <Input placeholder="Dealer Public Key Hash" />
                            <Button>Add Dealer</Button>
                        </div>
                    </div>
                    <div className="space-y-2">
                        {/* Dealer list would be mapped here */}
                        <div className="flex justify-between items-center p-2 bg-secondary rounded-lg">
                            <span className="text-sm truncate">addr1...</span>
                            <Button variant="destructive" size="sm">Remove</Button>
                        </div>
                    </div>
                </div>
            </CardContent>
        </Card>
    </div>
);

export default AdminDashboard;
